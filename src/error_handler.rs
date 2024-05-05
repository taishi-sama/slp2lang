use std::{collections::HashMap, fs::read_to_string, io, path::PathBuf};

use lalrpop_util::ParseError;
use source_span::{fmt::Formatter, Position, SourceBuffer, Span};

use crate::{ast::Loc, compiler::FileId, errors::CompilerErrors};

pub struct InfileLoc {
    pub span: Span,
}

impl InfileLoc {
    pub fn from_loc(loc: Loc, file_content: &str) -> Self {
        let metrics = &source_span::DEFAULT_METRICS; // characters metrics
        let mut pos = Position::new(0, 0);
        let mut span = Span::default();
        for (i, c) in file_content.char_indices() {
            if i < loc.begin {
                pos.shift(c, metrics)
            } else if i == loc.begin {
                span = pos.into();
            } else if i <= loc.end {
                span.push(c, metrics)
            }
        }
        InfileLoc { span }
    }
}

#[derive(Debug, Clone)]
pub struct ErrorHandler {
    errors: HashMap<PathBuf, Vec<CompilerErrors>>,
}

impl ErrorHandler {
    pub fn new() -> Self {
        ErrorHandler {
            errors: Default::default(),
        }
    }
    pub fn add_error(&mut self, error: CompilerErrors, fid_to_path: &HashMap<FileId, PathBuf>) {
        let loc = error.get_loc();
        let path = fid_to_path.get(&loc.fid).cloned().unwrap_or(PathBuf::new());
        self.errors
            .entry(path)
            .and_modify(|x| x.push(error.clone()))
            .or_insert_with(|| vec![error]);
    }
    pub fn add_syntax_error(
        &mut self,
        fid: FileId,
        error: &ParseError<usize, lalrpop_util::lexer::Token, &str>,
        fid_to_path: &HashMap<FileId, PathBuf>,
    ) {
        let res_error: CompilerErrors;
        match error {
            ParseError::InvalidToken { location } => {
                let loc = Loc::new(*location, *location, fid);
                res_error = CompilerErrors::SyntaxError(loc, "".into(), vec![])
            }
            ParseError::UnrecognizedEof { location, expected } => {
                let loc = Loc::new(*location, *location, fid);
                res_error = CompilerErrors::SyntaxError(
                    loc,
                    "EOF".into(),
                    expected.iter().map(|x| Self::translate_tokens(x)).collect(),
                )
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let loc = Loc::new(token.0, token.2, fid);
                res_error = CompilerErrors::SyntaxError(
                    loc,
                    Self::translate_tokens(token.1 .1),
                    expected.iter().map(|x| Self::translate_tokens(x)).collect(),
                )
            }
            ParseError::ExtraToken { token: _ } => todo!(),
            ParseError::User { error: _error } => todo!(),
        };
        self.add_error(res_error, fid_to_path)
    }
    pub fn translate_tokens(token: &str) -> String {
        match token {
            "Begin" => "begin".to_owned(),
            "End" => "end".to_owned(),
            "Procedure" => "procedure".to_owned(),
            "Function" => "function".to_owned(),
            "Var" => "var".to_owned(),
            "Array" => "array".to_owned(),
            "Of" => "of".to_owned(),
            "If" => "if".to_owned(),
            "Then" => "then".to_owned(),
            "Else" => "else".to_owned(),
            "Not" => "not".to_owned(),
            "Shl" => "shl".to_owned(),
            "Shr" => "shr".to_owned(),
            "And" => "and".to_owned(),
            "Or" => "or".to_owned(),
            "New" => "new".to_owned(),
            "Uses" => "uses".to_owned(),
            "External" => "external".to_owned(),
            "True" => "true".to_owned(),
            "False" => "false".to_owned(),
            "While" => "while".to_owned(),
            "Do" => "do".to_owned(),
            "TypeKW" => "type".to_owned(),
            "Class" => "class".to_owned(),
            "Record" => "record".to_owned(),
            "As" => "as".to_owned(),
            "Defer" => "defer".to_owned(),
            "For" => "for".to_owned(),
            "To" => "to".to_owned(),
            "Downto" => "downto".to_owned(),
            "Nil" => "nil".to_owned(),
            "CharLiteral" => "char literal".to_owned(),
            "StrLiteral" => "string literal".to_owned(),
            "Int" => "integer".to_owned(),
            "FullInt" => "any integer".to_owned(),
            "ID" => "identificator".to_owned(),
            
            a @ _ => a.to_owned()
            }
    }

    pub fn generate_error_message(err: &CompilerErrors) -> String {
        match err {
            CompilerErrors::BadType(_, ty) => format!("Unknown type: {}", ty),
            CompilerErrors::ExplicitTypeConversionError(_, from, to) =>  format!("Unable to convert type {} to type {}", from, to),
            CompilerErrors::UnknownFilename(_, n) => format!("Unknown file name: \"{}\". Ensure that file with name \"{}.slp\" exists, placed inside path directories and included in \"uses\" section.", n, n),
            CompilerErrors::FileIsNotInUses(_, n) => format!("File \"{}\" exists, but is not in usages list of this file.", n),
            CompilerErrors::ImplicitTypeConversionError(_, l, r) => 
                format!("Can't implicitly convert \"{l}\" to {r}."),
            CompilerErrors::IsNotLocalVariable(_) => format!("Expression is not a local variable."),
            CompilerErrors::InvalidArgumentCount(_, f, g, e) => {
                format!("Function \"{f}\" expects {e} arguments, but got {g} arguments.")
            },
            CompilerErrors::UnknownFunctionOrTypename(_, f) => {
                format!("\"{f}\" is not a valid type or function.")
            },
            CompilerErrors::InternalError(_, e) => {
                format!("Internal error: \"{e}\"")
            },
            CompilerErrors::IntExpected(_, q) => {
                format!("Expected any integer type but got type \"{q}\"")
            },
            CompilerErrors::FieldDoesntExists(_, f, t) => {
                format!("\"{t}\" doesn't have field or method with name \"{f}\"")
            },
            CompilerErrors::TypesNotSame(_, l, r) => 
                format!("Expected equal types, but got \"{l}\", \"{r}\""),
            CompilerErrors::LocalVariableDoesntExist(_, l) => {
                format!("There is no variable with name \"{l}\".")
            },
            CompilerErrors::NegateSymbolToUnsignedError(_) => {
                format!("Unsigned number literal can't be negative.")
            },
            CompilerErrors::InvalidSuffix(_, s) => {
                format!("Invalid suffix of literal: \"{s}\"")
            },
            CompilerErrors::InvalidNumber(_, s) => {
                format!("Invalid number: \"{s}\"")
            },
            CompilerErrors::ArrayExpected(_, s) => {
                format!("Expected any array type, got \"{s}\" type.")
            },
            CompilerErrors::InvalidNewOpArgumentCount(_, ty, g, e) => {
                format!("Operator \"new {ty}\" expected {e} arguments, {g} arguments got.")
            },
            CompilerErrors::InvalidTypeForNew(_, t) => {
                format!("Type \"{t}\" is not constuctable using \"new\".")
            },
            CompilerErrors::InvalidOperationForType(_, op, ty) => {
                format!("Operator {op} is not valid for type \"{ty}\".")
            },
            CompilerErrors::NotNullCheckableError(_, ty) => {
                format!("Type \"{ty}\" can't be checked on nil.")
            },
            CompilerErrors::SyntaxError(_, got, expected) => {
                format!("Syntax error: Got \"{}\", expected: {}", got, expected.join(", "))
            },
        }
    }
    //Returns true if has any errors inside
    pub fn display_errors(&self) -> io::Result<bool> {
        for (k, v) in &self.errors {
            let file = read_to_string(k)?;
            let chars = file.chars().map(|x| -> Result<char, ()> { Ok(x) });
            let metrics = source_span::DEFAULT_METRICS;
            let buffer = SourceBuffer::new(chars, source_span::Position::default(), metrics);
            let mut form = Formatter::new();

            buffer.iter().for_each(|_| ());
            for i in v {
                let s = InfileLoc::from_loc(i.get_loc(), &file);
                form.add(
                    s.span.clone(),
                    Some(Self::generate_error_message(i)),
                    source_span::fmt::Style::Error,
                );
            }
            let t = form.render(buffer.iter(), buffer.span().clone(), &metrics);
            println!("{}", t.unwrap())
        }
        Ok(!self.errors.is_empty())
    }
}
