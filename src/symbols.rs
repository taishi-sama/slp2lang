use std::{collections::{HashMap, HashSet}, mem, ops::Deref, rc::Rc, sync::Arc};

use crate::{
    ast::{ArgDecl, Declaration, Identificator, Loc, ProgramFile, RecordField, Type},
    compiler::{Compiler, FileId},
    errors::SemTreeBuildErrors,
    types::{SLPPrimitiveType, SLPType, StructType},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(pub String);

//TODO: Get all type alliases

//Declarations before any typechecking or name resolving, to make typechecks or name resolvings possible

#[derive(Debug, Clone)]
pub struct ContextSymbolResolver {
    pub main_file_symbols: Arc<Symbols>,
    pub deps_symbols: Vec<Arc<Symbols>>,
}
impl ContextSymbolResolver {
    pub fn new(main_file_symbols: Arc<Symbols>, deps_symbols: Vec<Arc<Symbols>>) -> Self {
        ContextSymbolResolver {
            main_file_symbols,
            deps_symbols,
        }
    }
    pub fn resolve(
        &self,
        id: &Identificator,
    ) -> Result<Option<(Id, FunctionDecl)>, SemTreeBuildErrors> {
        if id.path.is_empty() {
            let id = Id(id.name.clone());
            if let Some(sym) = self.main_file_symbols.func_decls.get(&id) {
                return Ok(Some((
                    self.main_file_symbols.canonical(&id, sym),
                    sym.clone(),
                )));
            }
            return Ok(None);
        } else if id.path.len() == 1 {
            let i = Id(id.name.clone());
            //println!("{:?}", self.deps_symbols);
            let res = self.deps_symbols.iter().find(|x| x.filename == id.path[0]);
            if let Some(syms) = res {
                if let Some(sym) = syms.func_decls.get(&i) {
                    return Ok(Some((syms.canonical(&i, sym), sym.clone())));
                }
                return Ok(None);
            } else {
                todo!("Can't find symbol {:?}", id)
            }
        } else {
            todo!()
        }
    }

    //fn canonical_name_of_id(id: &Identificator) -> String {
    //    let mut t = id.path.iter().fold(String::new(), |x, y| x + "$" + y);
    //    if t.is_empty() {
    //        t += "$";
    //    }
    //    t
    //}
}
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
    TypeAlias(&'a Type),
    StructDecl(PartialyResolvedRecord<'a>) 
}
#[derive(Debug, Clone, Default)]
struct PartialyResolvedRecord<'a> {
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
                            self.queue_to_resolve.insert((file, Id(name.clone())), ResolveTarget::TypeAlias(ty));
                        }
                        crate::ast::TypeDeclElement::RecordDeclare(_l, name, fields) => {
                            let mut prr = PartialyResolvedRecord::default();
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
    pub fn resolve(mut self) -> TypeSymbolResolver {
        let mut resolver = TypeSymbolResolver {
            internal: self.resolved,
            deps: self.compiler.deps.clone(),
            translation: self.compiler.filename_to_id.clone(),
        };
        let mut count = self.queue_to_resolve.len();
        while self.queue_to_resolve.len() > 0 {
            for ((fid, id), rt) in &mut self.queue_to_resolve {
                match rt {
                    ResolveTarget::TypeAlias(ty) => {
                        if let Ok(t) = resolver.from_ast_type(ty, fid) {
                            resolver
                                .internal
                                .insert((fid.clone(), id.clone()), SLPTypeDecl::TypeAlias(t));
                        } else {
                            self.next_round_resolve
                                .insert((fid.clone(), id.clone()), ResolveTarget::TypeAlias(ty));
                        }
                    },
                    ResolveTarget::StructDecl(rs) => {
                        let mut new_res = mem::take(&mut rs.resolved);
                        let mut new_to_be_resolved: HashMap<Id, &'a Type> = HashMap::new();
                        let field_order = rs.field_order.clone();
                        for (id, ty) in &rs.to_be_resolved {
                            if let Ok(t) = resolver.from_ast_type(ty, fid) {
                                println!("Field {} is resolved", id.0);
                                new_res.insert(id.clone(), t);
                            }
                            else {
                                println!("Field {} is not resolved", id.0);
                                new_to_be_resolved.insert(id.clone(), *ty);
                            }
                        }
                        let prr = PartialyResolvedRecord { resolved: new_res, to_be_resolved: new_to_be_resolved, field_order };
                        if !prr.to_be_resolved.is_empty() {
                            self.next_round_resolve.insert((fid.clone(), id.clone()), ResolveTarget::StructDecl(prr));
                        }
                        else {
                            let mut fields = vec![];
                            for i in prr.field_order.as_ref() {
                                fields.push((i.clone(), prr.resolved[i].clone()))
                            }
                            let st = StructType { name: id.clone(), fields };
                            resolver.internal.insert((fid.clone(), id.clone()), SLPTypeDecl::StructDecl(st));
                        }
                    },
                }
                
            }
            mem::swap(&mut self.queue_to_resolve, &mut self.next_round_resolve);
            self.next_round_resolve.clear();
            if count <= self.queue_to_resolve.len() {
                panic!("{count} <= {}", self.queue_to_resolve.len())
            }
            count = self.queue_to_resolve.len();
        }
        resolver
    }
}
#[derive(Debug, Clone)]
pub struct TypeSymbolResolver {
    pub internal: HashMap<(FileId, Id), SLPTypeDecl>,
    pub deps: Arc<HashMap<FileId, Vec<FileId>>>,
    pub translation: Arc<HashMap<String, FileId>>,
}
impl TypeSymbolResolver {
    pub fn from_ast_type(&self, ty: &Type, file: &FileId) -> Result<SLPType, SemTreeBuildErrors> {
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
                        _ => {
                            let id = (file.clone(), Id(t.name.clone()));
                            if let Some(ty) = self.internal.get(&id) {
                                return match ty {
                                    SLPTypeDecl::TypeAlias(ta) => Ok(ta.clone()),
                                    SLPTypeDecl::StructDecl(_) => todo!(),
                                };
                            } else {
                                return Err(SemTreeBuildErrors::TypeConversionError);
                            }
                        }
                    }))
                } else if t.path.len() == 1 {
                    let target_fid = self.translation.get(&t.path[0]).unwrap();
                    if self.deps.get(file).unwrap().contains(target_fid) {
                        let tmp = (target_fid.clone(), Id(t.name.clone()));
                        let ty = self.internal.get(&tmp);
                        if ty.is_none() {
                            println!("{:?}({}) not found", tmp, &t.path[0]);
                            return Err(SemTreeBuildErrors::TypeConversionError);
                        }

                        return match ty.unwrap() {
                            SLPTypeDecl::TypeAlias(ta) => Ok(ta.clone()),
                            SLPTypeDecl::StructDecl(_) => todo!(),
                        };
                    } else {
                        todo!("Error report: {} not in deps list", &t.path[0])
                    }
                } else {
                    todo!()
                }
            }
            Type::Pointer(t) => Ok(SLPType::Pointer(Box::new(self.from_ast_type(&t, file)?))),
            Type::DynArray(t) => Ok(SLPType::DynArray(Box::new(self.from_ast_type(&t, file)?))),
            //Insert offset to integer index on semtree building phase
            Type::FixedArray(b, e, t) => Ok(SLPType::FixedArray {
                ty: Box::new(self.from_ast_type(&t, file)?),
                size: (e - b + 1).try_into().unwrap(),
                index_offset: b.clone(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbols {
    pub filename: String,
    pub fileid: FileId,
    pub decls_order: Vec<Id>,
    pub func_decls: HashMap<Id, FunctionDecl>,
    pub type_decls: HashMap<Id, SLPTypeDecl>,
}

#[derive(Debug, Clone)]
pub enum SLPTypeDecl {
    TypeAlias(SLPType),
    StructDecl(StructType),
}
impl Symbols {
    pub fn canonical(&self, name: &Id, symbol: &FunctionDecl) -> Id {
        if name.0 == "main" {
            name.clone()
        } else {
            match symbol {
                FunctionDecl::FunctionDecl { .. } => Id(format!("{}${}", self.filename, name.0)),
                FunctionDecl::ExternFunctionDecl { .. } => Id(format!("{}", name.0)),
            }
        }
    }
    pub fn convert_typedecls(
        fid: &FileId,
        v: &[ArgDecl],
        res: &TypeSymbolResolver,
    ) -> Result<Vec<(Id, SLPType)>, SemTreeBuildErrors> {
        //v.iter()
        //    .map(|x| x.names.iter().map(|_| res.from_ast_type(&x.ty.ty, fid)))
        //    .flatten()
        //    .collect()
        let mut vardecls = vec![];
        for vardecl in v {
            let mut ty = res.from_ast_type(&vardecl.ty.ty, fid)?;
            if vardecl.var_param {
                ty = SLPType::AutoderefPointer(Box::new(ty));
            }
            for decl in &vardecl.names {
                vardecls.push((Id(decl.clone()), ty.clone()))
            }
        }
        Ok(vardecls)
    }

    pub fn new(
        filename: &str,
        pf: &ProgramFile,
        fid: FileId,
        res: &TypeSymbolResolver,
    ) -> Result<Symbols, SemTreeBuildErrors> {
        let mut decls_order = vec![];
        let mut decls = HashMap::new();
        for dec in &pf.declarations {
            match dec {
                crate::ast::Declaration::Function(f) => {
                    decls_order.push(Id(f.function_name.to_string()));
                    decls.insert(
                        Id(f.function_name.to_string()),
                        FunctionDecl::FunctionDecl {
                            loc: f.loc,
                            input: Self::convert_typedecls(&fid, &f.function_args, res)?
                                .into_iter()
                                .map(|x| x.1)
                                .collect(),
                            output: res.from_ast_type(&f.return_arg.ty, &fid)?,
                        },
                    );
                }
                crate::ast::Declaration::ExternFunction(f) => {
                    decls_order.push(Id(f.function_name.to_string()));
                    decls.insert(
                        Id(f.function_name.to_string()),
                        FunctionDecl::ExternFunctionDecl {
                            loc: f.loc,
                            input: Self::convert_typedecls(&fid, &f.function_args, res)?
                                .into_iter()
                                .map(|x| x.1)
                                .collect(),
                            output: res.from_ast_type(&f.return_arg.ty, &fid)?,
                        },
                    );
                }
                crate::ast::Declaration::TypeDeclSection(_) => {
                    //TODO
                }
            }
        }
        Ok(Symbols {
            filename: filename.to_string(),
            decls_order,
            func_decls: decls,
            type_decls: Default::default(),
            fileid: fid,
        })
    }
}
#[derive(Debug, Clone)]
pub enum FunctionDecl {
    //There no difference in extern and not extern functions for typechecking or name resolving
    FunctionDecl {
        loc: Loc,
        input: Vec<SLPType>,
        output: SLPType,
    },
    ExternFunctionDecl {
        loc: Loc,
        input: Vec<SLPType>,
        output: SLPType,
    },
}
