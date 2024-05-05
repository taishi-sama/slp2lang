use thiserror::Error;

use crate::ast::Loc;
#[derive(Debug, Error, Clone)]
pub enum CompilerErrors {
    #[error("")]
    SyntaxError(Loc, String, Vec<String>),
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

impl CompilerErrors {
    pub fn get_loc(&self) -> Loc {
        match self {
            CompilerErrors::BadType(l, _) => l.clone(),
            CompilerErrors::ExplicitTypeConversionError(l, _, _) => l.clone(),
            CompilerErrors::UnknownFilename(l, _) => l.clone(),
            CompilerErrors::FileIsNotInUses(l, _) => l.clone(),
            CompilerErrors::ImplicitTypeConversionError(l, _, _) => l.clone(),
            CompilerErrors::IsNotLocalVariable(l) => l.clone(),
            CompilerErrors::InvalidArgumentCount(l, _, _, _) => l.clone(),
            CompilerErrors::UnknownFunctionOrTypename(l, _) => l.clone(),
            CompilerErrors::InternalError(l, _) => l.clone(),
            CompilerErrors::IntExpected(l, _) => l.clone(),
            CompilerErrors::FieldDoesntExists(l, _, _) => l.clone(),
            CompilerErrors::TypesNotSame(l, _, _) => l.clone(),
            CompilerErrors::LocalVariableDoesntExist(_, _) => todo!(),
            CompilerErrors::NegateSymbolToUnsignedError(l) => l.clone(),
            CompilerErrors::InvalidSuffix(l, _) => l.clone(),
            CompilerErrors::InvalidNumber(l, _) => l.clone(),
            CompilerErrors::ArrayExpected(l, _) => l.clone(),
            CompilerErrors::InvalidNewOpArgumentCount(l, _, _, _) => l.clone(),
            CompilerErrors::InvalidTypeForNew(l, _) => l.clone(),
            CompilerErrors::InvalidOperationForType(l, _, _) => l.clone(),
            CompilerErrors::NotNullCheckableError(l, _) => l.clone(),
            CompilerErrors::SyntaxError(l, _, _) => l.clone(),
        }
    }
}
