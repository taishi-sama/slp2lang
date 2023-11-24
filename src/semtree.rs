use std::collections::HashMap;

use crate::ast::{ProgramFile, Loc, Type};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(String);


//TODO: Get all type alliases 

//Declarations before any typechecking or name resolving, to make typechecks or name resolvings possible
pub struct RawDeclarations {
    filename: String,
    decls_order: Vec<Id>,
    decls: HashMap<Id, RawDeclaration> 
}
impl RawDeclarations {
    pub fn new(filename: &str, pf: &ProgramFile) -> RawDeclarations {
        for dec in &pf.declarations {
            match dec {
                crate::ast::Declaration::Function(f) => todo!(),
                crate::ast::Declaration::ExternFunction(f) => todo!(),
                crate::ast::Declaration::TypeDeclSection(_) => todo!(),
            }
        }
        RawDeclarations { filename: filename.to_string(), decls_order: todo!(), decls: todo!() }
    }
}
pub enum RawDeclaration {
    //There no difference in extern and not extern functions for typechecking or name resolving
    FunctionDecl(Loc, )
}
pub struct ProgramFileTarget {
    decls: Vec<Declaration>
}
pub enum Declaration {
    
}