use std::collections::HashMap;

use crate::{ast::{ProgramFile, Loc}, types::SLPType};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(String);


//TODO: Get all type alliases 

//Declarations before any typechecking or name resolving, to make typechecks or name resolvings possible
#[derive(Debug, Clone)]
pub struct RawSymbols {
    filename: String,
    decls_order: Vec<Id>,
    decls: HashMap<Id, RawSymbol> 
}
impl RawSymbols {
    pub fn new(filename: &str, pf: &ProgramFile) -> RawSymbols {
        let mut decls_order = vec![];
        let mut decls = HashMap::new();
        for dec in &pf.declarations {
            match dec {
                crate::ast::Declaration::Function(f) => {
                    decls_order.push(Id(f.function_name.to_string()));
                    decls.insert(Id(f.function_name.to_string()), 
                        RawSymbol::FunctionDecl { loc: f.loc, 
                            input: f.function_args.iter()
                            .map(|x|
                                x.names.iter()
                                    .map(|_| SLPType::from_ast_type(&x.ty.ty)
                                )).flatten().collect(), 
                            output: SLPType::from_ast_type(&f.return_arg.ty) 
                        });
                },
                crate::ast::Declaration::ExternFunction(f) => {
                    decls_order.push(Id(f.function_name.to_string()));
                    decls.insert(Id(f.function_name.to_string()), 
                        RawSymbol::FunctionDecl { loc: f.loc, 
                            input: f.function_args.iter()
                            .map(|x|
                                x.names.iter()
                                    .map(|_| SLPType::from_ast_type(&x.ty.ty)
                                )).flatten().collect(), 
                            output: SLPType::from_ast_type(&f.return_arg.ty) 
                        });
                },
                crate::ast::Declaration::TypeDeclSection(_) => todo!(),
            }
        }
        RawSymbols { filename: filename.to_string(), decls_order, decls }
    }
}
#[derive(Debug, Clone)]
pub enum RawSymbol {
    //There no difference in extern and not extern functions for typechecking or name resolving
    FunctionDecl{
        loc: Loc, 
        input: Vec<SLPType>,
        output: SLPType,
    }
}