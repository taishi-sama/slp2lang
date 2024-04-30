use thiserror::Error;

use crate::ast::Loc;
#[derive(Debug, Error, Clone)]
pub enum SemTreeBuildErrors {
    #[error("On {0} unknown type: {1}")]
    BadType(Loc, String),
    #[error("Error while converting types")]
    TypeConversionError(Loc, String, String),

}
