use thiserror::Error;

use crate::ast::Loc;
#[derive(Debug, Error, Clone)]
pub enum SemTreeBuildErrors {
    #[error("On {0} unknown type: {1}")]
    BadType(Loc, String),
    #[error("Error while converting types")]
    TypeConversionError(Loc, String, String),

}

impl SemTreeBuildErrors {
    pub fn get_loc(&self) -> Loc {
        match self {
            SemTreeBuildErrors::BadType(l, _) => l.clone(),
            SemTreeBuildErrors::TypeConversionError(l, _, _) => l.clone(),
        }
    }
}