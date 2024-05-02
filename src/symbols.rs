use std::{cell::RefCell, collections::{HashMap, HashSet}, mem, rc::Rc, sync::Arc};

use crate::{
    ast::{ArgDecl, Declaration, Identificator, Loc, ProgramFile, Type}, buildins::BuildInModule, compiler::{Compiler, FileId}, errors::SemTreeBuildErrors, types::{SLPPrimitiveType, SLPType, StructType}
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(pub String);

//TODO: Get all type alliases

//Declarations before any typechecking or name resolving, to make typechecks or name resolvings possible


#[derive(Debug, Clone)]
pub struct TypeResolverGenerator<'a, 'b> {
    pub compiler: &'b Compiler,
    queue_to_resolve: HashMap<(FileId, Id), ResolveTarget<'a>>,
    next_round_resolve: HashMap<(FileId, Id), ResolveTarget<'a>>,
    pub declared_structs: HashSet<(FileId, Id)>,
    pub resolved: HashMap<(FileId, Id), SLPTypeDecl>,
}
#[derive(Debug, Clone)]
enum ResolveTarget<'a> {
    TypeAlias(Loc, &'a Type),
    StructDecl(PartialyResolvedRecord<'a>) 
}
#[derive(Debug, Clone)]
struct PartialyResolvedRecord<'a> {
    pub loc: Loc,
    pub is_class: bool,
    pub resolved: HashMap<Id, SLPType>,
    pub to_be_resolved: HashMap<Id, &'a Type>,
    pub field_order: Rc<Vec<Id>>,
}
impl<'a, 'b> TypeResolverGenerator<'a, 'b> {
    pub fn new(compiler: &'b Compiler) -> Self {
        Self {
            compiler,
            queue_to_resolve: Default::default(),
            next_round_resolve: Default::default(),
            resolved: Default::default(),
            declared_structs: Default::default(),
        }
    }
    pub fn fill(&mut self, pf: &'a ProgramFile, file: FileId) {
        for decs in &pf.declarations {
            if let Declaration::TypeDeclSection(section) = decs {
                for ty_decl in &section.decls {
                    match ty_decl {
                        crate::ast::TypeDeclElement::TypeAlias(_l, name, ty) => {
                            self.queue_to_resolve.insert((file, Id(name.clone())), ResolveTarget::TypeAlias(_l.clone(), ty));
                        }
                        crate::ast::TypeDeclElement::RecordDeclare(_l, name, fields, rec_ty) => {
                            let mut prr = PartialyResolvedRecord{
                                loc: _l.clone(),
                                is_class: false,
                                resolved: Default::default(),
                                to_be_resolved: Default::default(),
                                field_order: Default::default(),
                            };
                            prr.is_class = match rec_ty {
                                crate::ast::RecordType::Record => false,
                                crate::ast::RecordType::Class => true,
                            };
                            for i in fields {
                                Rc::get_mut(&mut prr.field_order).unwrap().push(Id(i.id.clone()));
                                assert!(prr.to_be_resolved.insert(Id(i.id.clone()), &i.ty.ty).is_none());
                            }
                            
                            self.queue_to_resolve.insert((file, Id(name.clone())), ResolveTarget::StructDecl(prr));
                        },
                    }
                }
            }
        }
    }
    pub fn resolve(mut self) -> Result<GlobalSymbolResolver, Vec<SemTreeBuildErrors>> {
        let mut resolver = GlobalSymbolResolver {
            types: self.resolved,
            deps: self.compiler.deps.clone(),
            filename_translation: self.compiler.filename_to_id.clone(),
            reverse_filename_translation: self.compiler.id_to_filename.clone(),
            functions: Default::default(),
            function_declare_order: Default::default(),
        };
        let mut error_list = vec![];
        let mut count = self.queue_to_resolve.len();
        while self.queue_to_resolve.len() > 0 {
            for ((fid, id), rt) in &mut self.queue_to_resolve {
                match rt {
                    ResolveTarget::TypeAlias(loc, ty) => {
                        match resolver.from_ast_type(&loc, ty, fid) {
                            Ok(t) => {
                                resolver
                                .types
                                .insert((fid.clone(), id.clone()), SLPTypeDecl::TypeAlias(t));
                            },
                            Err(err) => {
                                error_list.push(err);
                                self.next_round_resolve
                                    .insert((fid.clone(), id.clone()), ResolveTarget::TypeAlias(loc.clone(), ty));
                            },
                        }
                    },
                    ResolveTarget::StructDecl(rs) => {
                        let mut new_res = mem::take(&mut rs.resolved);
                        let mut new_to_be_resolved: HashMap<Id, &'a Type> = HashMap::new();
                        let field_order = rs.field_order.clone();
                        for (id, ty) in &rs.to_be_resolved {
                            match resolver.from_ast_type(&rs.loc,  ty, fid) {
                                Ok(t) => {
                                    new_res.insert(id.clone(), t);
                                },
                                Err(err) => {
                                    error_list.push(err);
                                    new_to_be_resolved.insert(id.clone(), *ty);
                                },
                            }
                        }
                        let prr = PartialyResolvedRecord { resolved: new_res, to_be_resolved: new_to_be_resolved, field_order, is_class: rs.is_class, loc: rs.loc.clone() };
                        if !prr.to_be_resolved.is_empty() {
                            self.next_round_resolve.insert((fid.clone(), id.clone()), ResolveTarget::StructDecl(prr));
                        }
                        else {
                            let mut fields = vec![];
                            let mut is_copiable = true;
                            for i in prr.field_order.as_ref() {
                                let field_ty = prr.resolved[i].clone();
                                is_copiable = is_copiable && field_ty.is_trivially_copiable();

                                fields.push((i.clone(), field_ty))
                            }
                            let st = StructType { name: id.clone(), fields, is_class: prr.is_class, is_copiable };
                            resolver.types.insert((fid.clone(), id.clone()), SLPTypeDecl::StructDecl(st));
                        }
                    },
                }
                
            }
            mem::swap(&mut self.queue_to_resolve, &mut self.next_round_resolve);
            self.next_round_resolve.clear();
            if count <= self.queue_to_resolve.len() {
                return Err(error_list);
                //panic!("{count} <= {}", self.queue_to_resolve.len())
            } else {
                error_list.clear();
            }
            count = self.queue_to_resolve.len();
        }
        Ok(resolver)
    }
}
#[derive(Debug, Clone)]
pub struct GlobalSymbolResolver {
    pub types: HashMap<(FileId, Id), SLPTypeDecl>,
    pub function_declare_order: HashMap<FileId, Vec<Id>>,
    pub functions: HashMap<(FileId, Id), FunctionDecl>,
    pub deps: Arc<HashMap<FileId, Vec<FileId>>>,
    pub filename_translation: Arc<HashMap<String, FileId>>,
    pub reverse_filename_translation: Arc<HashMap<FileId, String>>,
}
impl GlobalSymbolResolver {
    pub fn get_fileid_checked(&self, loc: &Loc, name: &str) -> Result<FileId, SemTreeBuildErrors> {
        let target_fid = self.filename_translation.get(name);
        match target_fid {
            Some(f) => Ok(f.clone()),
            None => Err(SemTreeBuildErrors::UnknownFilename(loc.clone(), name.to_owned())),
        }
    }
    pub fn is_in_dep_list_checked(&self, loc: &Loc, checked: FileId, dep: FileId) -> Result<(), SemTreeBuildErrors> {
        if self.deps.get(&dep).unwrap().contains(&checked) {
            Ok(())
        } else {
            let name = self.reverse_filename_translation[&checked].clone();
            Err(SemTreeBuildErrors::FileIsNotInUses(loc.clone(), name))
        }
    }
    pub fn canonical_functions(&self, fid: &FileId, id: &Id) -> Id {
        let fn_type = self.functions.get(&(fid.clone(), id.clone())).unwrap();
        if id.0 == "main" {
            id.clone()
        } else {
            match fn_type {
                FunctionDecl::FunctionDecl { .. } => Id(format!("{}${}", self.reverse_filename_translation[fid], id.0)),
                FunctionDecl::ExternFunctionDecl { .. } => Id(format!("{}", id.0)),
            }
        }
    }
    pub fn fill_function_decl(&mut self, pf: &ProgramFile, fid: &FileId) -> Result<(), SemTreeBuildErrors> {
        let mut decls = HashMap::new();
        for dec in &pf.declarations {
            match dec {
                crate::ast::Declaration::Function(f) => {
                    let i = Id(f.function_name.to_string());
                    self.function_declare_order.entry(fid.clone()).and_modify(|x|x.push(i.clone())).or_insert(vec![i.clone()]);
                    decls.insert(
                        (fid.clone(), i),
                        FunctionDecl::FunctionDecl {
                            loc: f.loc,
                            input: self.convert_typedecls(&f.loc, &fid, &f.function_args)?,
                            output: self.from_ast_type(&f.loc, &f.return_arg.ty, &fid)?,
                        },
                    );
                }
                crate::ast::Declaration::ExternFunction(f) => {
                    let i = Id(f.function_name.to_string());
                    self.function_declare_order.entry(fid.clone()).and_modify(|x|x.push(i.clone())).or_insert(vec![i.clone()]);
                    decls.insert(
                        (fid.clone(), i),
                        FunctionDecl::ExternFunctionDecl {
                            loc: f.loc,
                            input: self.convert_typedecls(&f.loc, &fid, &f.function_args)?,
                            output: self.from_ast_type(&f.loc, &f.return_arg.ty, &fid)?,
                        },
                    );
                }
                crate::ast::Declaration::TypeDeclSection(_) => {
                    //TODO
                }
            }
        }
        self.functions.extend(decls);
        Ok(())
    }
    
    fn convert_typedecls(&self,
        loc: &Loc,
        fid: &FileId,
        v: &[ArgDecl]
    ) -> Result<Vec<(Id, SLPType)>, SemTreeBuildErrors> {
        //v.iter()
        //    .map(|x| x.names.iter().map(|_| res.from_ast_type(&x.ty.ty, fid)))
        //    .flatten()
        //    .collect()
        let mut vardecls = vec![];
        for vardecl in v {
            let mut ty = self.from_ast_type(loc, &vardecl.ty.ty, fid)?;
            if vardecl.var_param {
                ty = SLPType::AutoderefPointer(Box::new(ty));
            }
            for decl in &vardecl.names {
                vardecls.push((Id(decl.clone()), ty.clone()))
            }
        }
        Ok(vardecls)
    }
    pub fn canonical_structures_name(&self, fid: &FileId, id: &Id) -> Id {
        todo!()
    }
    pub fn get_struct(&self, fid: &String, id: &Id) -> Result<Option<&StructType>, SemTreeBuildErrors> {
        let f_n = self.filename_translation.as_ref().get(fid).unwrap();
        let t = &self.types[&(f_n.clone(), id.clone())];
        if let SLPTypeDecl::StructDecl(sd) = t {

            Ok(Some(sd))
        }
        else {
            Ok(None)
        }
    }
    pub fn resolve_funccall(&self, loc: &Loc, fid: &FileId, id: &Identificator) -> Result<Option<(Id, FunctionDecl)>, SemTreeBuildErrors>  {
        if id.path.is_empty() {
            let id = Id(id.name.clone());
            if let Some(sym) = self.functions.get(&(fid.clone(), id.clone())) {
                return Ok(Some((
                    self.canonical_functions(fid,&id),
                    sym.clone(),
                )));
            }
            return Ok(None);
        } else if id.path.len() == 1 {
            let i = Id(id.name.clone());
            //println!("{:?}", self.deps_symbols);
            let target_fid = self.get_fileid_checked(loc, &id.path[0])?;
            self.is_in_dep_list_checked(loc, target_fid, *fid)?;

            if let Some(sym) = self.functions.get(&(target_fid.clone(), i.clone())) {
                return Ok(Some((
                    self.canonical_functions(&target_fid, &i),
                    sym.clone(),
                )));
            }
            else {
                return Ok(None);
            }
        } else {
            todo!()
        }
    }
    pub fn from_ast_type(&self, loc: &Loc, ty: &Type, file: &FileId) -> Result<SLPType, SemTreeBuildErrors> {
        match ty {
            Type::Primitive(t) => {
                if t.path.is_empty() {
                    Ok(SLPType::PrimitiveType(match &t.name[..] {
                        "int8" => SLPPrimitiveType::Int8,
                        "int16" => SLPPrimitiveType::Int16,
                        "int32" => SLPPrimitiveType::Int32,
                        "int64" => SLPPrimitiveType::Int64,
                        "isize" => SLPPrimitiveType::ISize,
                        "uint8" => SLPPrimitiveType::Uint8,
                        "uint16" => SLPPrimitiveType::Uint16,
                        "uint32" => SLPPrimitiveType::Uint32,
                        "uint64" => SLPPrimitiveType::Uint64,
                        "usize" => SLPPrimitiveType::USize,
                        "string" => SLPPrimitiveType::String,
                        "bool" => SLPPrimitiveType::Bool,
                        "void" => SLPPrimitiveType::Void,
                        "char" => SLPPrimitiveType::Char,
                        a @ _ => {
                            let id = (file.clone(), Id(t.name.clone()));
                            let fname = self.reverse_filename_translation.as_ref().get(&id.0).unwrap();
                            if let Some(ty) = self.types.get(&id) {
                                return match ty {
                                    SLPTypeDecl::TypeAlias(ta) => Ok(ta.clone()),
                                    SLPTypeDecl::StructDecl(d) => if d.is_class {
                                        Ok(SLPType::RefCounter(Box::new(SLPType::Struct(fname.clone(), id.1, d.is_copiable))))
                                    } else {
                                        Ok(SLPType::Struct(fname.clone(), id.1, d.is_copiable))
                                    },
                                };
                            } else {
                                return Err(SemTreeBuildErrors::BadType(loc.clone(), a.to_owned()));
                            }
                        }
                    }))
                } else if t.path.len() == 1 {
                    let target_fid = self.get_fileid_checked(loc, &t.path[0])?;
                    self.is_in_dep_list_checked(loc, target_fid, *file)?;
                    
                    let tmp = (target_fid.clone(), Id(t.name.clone()));
                    let ty = self.types.get(&tmp);
                    if ty.is_none() {
                        println!("{:?}({}) not found", tmp, &t.path[0]);
                        return Err(SemTreeBuildErrors::BadType(loc.clone(), t.to_string()));
                    }
                    let fname = self.reverse_filename_translation.as_ref().get(&tmp.0).unwrap();
                    return match ty.unwrap() {
                        SLPTypeDecl::TypeAlias(ta) => Ok(ta.clone()),
                        SLPTypeDecl::StructDecl(d) => if d.is_class {
                            Ok(SLPType::RefCounter(Box::new(SLPType::Struct(fname.clone(), tmp.1.clone(), d.is_copiable))))
                        } else {
                            Ok(SLPType::Struct(fname.clone(), tmp.1.clone(), d.is_copiable))
                        },
                    };
                } else {
                    todo!()
                }
            }
            Type::Pointer(t) => Ok(SLPType::Pointer(Box::new(self.from_ast_type(loc, &t, file)?))),
            Type::DynArray(t) => Ok(SLPType::RefCounter(Box::new(SLPType::DynArray(Box::new(self.from_ast_type(loc, &t, file)?))))),
            //Insert offset to integer index on semtree building phase
            Type::FixedArray(b, e, t) => Ok(SLPType::FixedArray {
                ty: Box::new(self.from_ast_type(loc, &t, file)?),
                size: (e - b + 1).try_into().unwrap(),
                index_offset: b.clone(),
            }),
        }
    }
}


#[derive(Debug, Clone)]
pub enum SLPTypeDecl {
    TypeAlias(SLPType),
    StructDecl(StructType),
}

#[derive(Debug, Clone)]
pub enum FunctionDecl {
    //There no difference in extern and not extern functions for typechecking or name resolving
    FunctionDecl {
        loc: Loc,
        input: Vec<(Id, SLPType)>,
        output: SLPType,
    },
    ExternFunctionDecl {
        loc: Loc,
        input: Vec<(Id, SLPType)>,
        output: SLPType,
    },
}
