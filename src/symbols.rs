use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::{ArgDecl, Identificator, Loc, ProgramFile},
    errors::SemTreeBuildErrors,
    types::SLPType,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(pub String);

//TODO: Get all type alliases

//Declarations before any typechecking or name resolving, to make typechecks or name resolvings possible

#[derive(Debug, Clone)]
pub struct ContextSymbolResolver {
    pub main_file_symbols: Arc<RawSymbols>,
    pub deps_symbols: Vec<Arc<RawSymbols>>,
}
impl ContextSymbolResolver {
    pub fn new(main_file_symbols: Arc<RawSymbols>, deps_symbols: Vec<Arc<RawSymbols>> ) -> Self {
        ContextSymbolResolver { main_file_symbols, deps_symbols }
    }
    pub fn resolve(&self, id: &Identificator) -> Result<Option<(Id, RawSymbol)>, SemTreeBuildErrors> {
        if id.path.is_empty() {
            let id = Id(id.name.clone());
            if let Some(sym) = self.main_file_symbols.decls.get(&id) {
                return Ok(Some((self.main_file_symbols.canonical(&id, sym), sym.clone())));
            }
            todo!()
        }
        else if id.path.len() == 1 {
            let i = Id(id.name.clone());
            //println!("{:?}", self.deps_symbols);
            let res = self.deps_symbols.iter().find(|x|x.filename == id.path[0]);
            if let Some(syms) = res {
                if let Some(sym) = syms.decls.get(&i) {
                    return Ok(Some((syms.canonical(&i, sym), sym.clone())));
                }
                else {todo!()}
            }
            else {todo!("Can't find symbol {:?}", id)}
        }
        else {todo!()}

    }
    
    fn canonical_name_of_id(id: &Identificator) -> String {
        let mut t = id.path.iter().fold(String::new(), |x, y| x + "$" + y);
        if t.is_empty() {
            t += "$";
        }
        t
    }
}

#[derive(Debug, Clone)]
pub struct RawSymbols {
    pub filename: String,
    pub decls_order: Vec<Id>,
    pub decls: HashMap<Id, RawSymbol>,
}

impl RawSymbols {
    pub fn canonical(&self, name: &Id, symbol: &RawSymbol) -> Id {
        if name.0 == "main" {
            name.clone()
        }
        else {
            match symbol {
                RawSymbol::FunctionDecl { .. } => Id(format!("{}${}", self.filename, name.0)),
                RawSymbol::ExternFunctionDecl { .. } => Id(format!("{}", name.0)),
            }
        }
    }
    fn convert_typedecls(v: &[ArgDecl]) -> Result<Vec<SLPType>, SemTreeBuildErrors> {
        v.iter()
            .map(|x| x.names.iter().map(|_| SLPType::from_ast_type(&x.ty.ty)))
            .flatten()
            .collect()
    }
    fn get_canonical_name(filename: &str, name: &str, is_extern: bool) -> String {
        todo!()
    }
    pub fn new(filename: &str, pf: &ProgramFile) -> Result<RawSymbols, SemTreeBuildErrors> {
        let mut decls_order = vec![];
        let mut decls = HashMap::new();
        for dec in &pf.declarations {
            match dec {
                crate::ast::Declaration::Function(f) => {
                    decls_order.push(Id(f.function_name.to_string()));
                    decls.insert(
                        Id(f.function_name.to_string()),
                        RawSymbol::FunctionDecl {
                            loc: f.loc,
                            input: Self::convert_typedecls(&f.function_args)?,
                            output: SLPType::from_ast_type(&f.return_arg.ty)?,
                        },
                    );
                }
                crate::ast::Declaration::ExternFunction(f) => {
                    decls_order.push(Id(f.function_name.to_string()));
                    decls.insert(
                        Id(f.function_name.to_string()),
                        RawSymbol::ExternFunctionDecl {
                            loc: f.loc,
                            input: Self::convert_typedecls(&f.function_args)?,
                            output: SLPType::from_ast_type(&f.return_arg.ty)?,
                        },
                    );
                }
                crate::ast::Declaration::TypeDeclSection(_) => todo!(),
            }
        }
        Ok(RawSymbols {
            filename: filename.to_string(),
            decls_order,
            decls,
        })
    }
}
#[derive(Debug, Clone)]
pub enum RawSymbol {
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
    }
}
