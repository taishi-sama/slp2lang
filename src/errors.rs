use thiserror::Error;

use crate::ast::Loc;
#[derive(Debug, Error, Clone)]
pub enum SemTreeBuildErrors {
    #[error("On {0} unknown type: {1}")]
    BadType(Loc, String),
    #[error("")]
    IntExpected(Loc, String),
    #[error("")]
    TypesNotSame(Loc, String, String),
    #[error("Error while converting types")]
    ExplicitTypeConversionError(Loc, String, String),
    #[error("Error while converting types")]
    ImplicitTypeConversionError(Loc, String, String),
    #[error("Error while converting types")]
    IsNotLocalVariable(Loc),
    #[error("Error while converting types")]
    UnknownFunctionOrTypename(Loc, String),
    #[error("Error while converting types")]
    LocalVariableDoesntExist(Loc, String),
    #[error("")]
    ///Field name, Type name
    FieldDoesntExists(Loc, String, String),
    #[error("In")]
    InvalidArgumentCount(Loc, String, usize, usize),
    #[error("")]
    InvalidNewOpArgumentCount(Loc, String, usize, usize),
    #[error("")]
    InvalidTypeForNew(Loc, String),
    #[error("")]
    ///Opetarion, Type
    InvalidOperationForType(Loc, String, String),
    #[error("")]
    NotNullCheckableError(Loc, String),

    #[error("")]
    NegateSymbolToUnsignedError(Loc),
    #[error("")]
    InvalidSuffix(Loc, String),
    #[error("")]
    InvalidNumber(Loc, String),
    #[error("")]
    ArrayExpected(Loc, String),
    #[error("")]
    UnknownFilename(Loc, String),
    #[error("")]
    FileIsNotInUses(Loc, String),
    #[error("")]
    InternalError(Loc, String),
}

impl SemTreeBuildErrors {
    pub fn get_loc(&self) -> Loc {
        match self {
            SemTreeBuildErrors::BadType(l, _) => l.clone(),
            SemTreeBuildErrors::ExplicitTypeConversionError(l, _, _) => l.clone(),
            SemTreeBuildErrors::UnknownFilename(l, _) => l.clone(),
            SemTreeBuildErrors::FileIsNotInUses(l, _) => l.clone(),
            SemTreeBuildErrors::ImplicitTypeConversionError(l, _, _) => l.clone(),
            SemTreeBuildErrors::IsNotLocalVariable(l) => l.clone(),
            SemTreeBuildErrors::InvalidArgumentCount(l, _, _, _) => l.clone(),
            SemTreeBuildErrors::UnknownFunctionOrTypename(l, _) => l.clone(),
            SemTreeBuildErrors::InternalError(l, _) => l.clone(),
            SemTreeBuildErrors::IntExpected(l, _) => l.clone(),
            SemTreeBuildErrors::FieldDoesntExists(l, _, _) => l.clone(),
            SemTreeBuildErrors::TypesNotSame(l, _, _) => l.clone(),
            SemTreeBuildErrors::LocalVariableDoesntExist(_, _) => todo!(),
            SemTreeBuildErrors::NegateSymbolToUnsignedError(l) => l.clone(),
            SemTreeBuildErrors::InvalidSuffix(l, _) => l.clone(),
            SemTreeBuildErrors::InvalidNumber(l, _) => l.clone(),
            SemTreeBuildErrors::ArrayExpected(l, _) => l.clone(),
            SemTreeBuildErrors::InvalidNewOpArgumentCount(l, _, _, _) => l.clone(),
            SemTreeBuildErrors::InvalidTypeForNew(l, _) => l.clone(),
            SemTreeBuildErrors::InvalidOperationForType(l, _, _) => l.clone(),
            SemTreeBuildErrors::NotNullCheckableError(l, _) => l.clone(),
        }
    }
}