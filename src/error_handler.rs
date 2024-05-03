use std::{collections::HashMap, fmt::format, fs::read_to_string, io, path::PathBuf};

use source_span::{fmt::Formatter, Position, SourceBuffer, Span};

use crate::{ast::Loc, compiler::FileId, errors::SemTreeBuildErrors};




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
    errors: HashMap<PathBuf, Vec<SemTreeBuildErrors>>
}

impl ErrorHandler {
    pub fn new() -> Self{
        ErrorHandler {errors: Default::default()}
    }
    pub fn add_error(&mut self, error: SemTreeBuildErrors, fid_to_path: &HashMap<FileId, PathBuf>)  {
        let loc = error.get_loc();
        let path = fid_to_path.get(&loc.fid).cloned().unwrap_or(PathBuf::new());
        self.errors.entry(path).and_modify(|x|x.push(error.clone())).or_insert_with(||vec![error]);
    }
    pub fn generate_error_message(err: &SemTreeBuildErrors) -> String {
        match err {
            SemTreeBuildErrors::BadType(_, ty) => format!("Unknown type: {}", ty),
            SemTreeBuildErrors::ExplicitTypeConversionError(_, from, to) =>  format!("Unable to convert type {} to type {}", from, to),
            SemTreeBuildErrors::UnknownFilename(_, n) => format!("Unknown file name: \"{}\". Ensure that file with name \"{}.slp\" exists, placed inside path directories and included in \"uses\" section.", n, n),
            SemTreeBuildErrors::FileIsNotInUses(_, n) => format!("File \"{}\" exists, but is not in usages list of this file.", n),
            SemTreeBuildErrors::ImplicitTypeConversionError(_, _, _) => todo!(),
            SemTreeBuildErrors::IsNotLocalVariable(_) => todo!(),
            SemTreeBuildErrors::InvalidArgumentCount(_, _, _, _) => todo!(),
            SemTreeBuildErrors::UnknownFunctionOrTypename(_, _) => todo!(),
            SemTreeBuildErrors::InternalError(_, _) => todo!(),
            SemTreeBuildErrors::IntExpected(_, _) => todo!(),
            SemTreeBuildErrors::FieldDoesntExists(_, _, _) => todo!(),
            SemTreeBuildErrors::TypesNotSame(_, _, _) => todo!(),
            SemTreeBuildErrors::LocalVariableDoesntExist(_, _) => todo!(),
            SemTreeBuildErrors::NegateSymbolToUnsignedError(_) => todo!(),
            SemTreeBuildErrors::InvalidSuffix(_, _) => todo!(),
            SemTreeBuildErrors::InvalidNumber(_, _) => todo!(),
            SemTreeBuildErrors::ArrayExpected(_, _) => todo!(),
            SemTreeBuildErrors::InvalidNewOpArgumentCount(_, _, _, _) => todo!(),
            SemTreeBuildErrors::InvalidTypeForNew(_, _) => todo!(),
            SemTreeBuildErrors::InvalidOperationForType(_, _, _) => todo!(),
            SemTreeBuildErrors::NotNullCheckableError(_, _) => todo!(),
        }
    }   
    //Returns true if has any errors inside
    pub fn display_errors(&self) -> io::Result<bool> {
        for (k, v) in &self.errors {
            let file = read_to_string(k)?;         
            let chars = file.chars().map(|x|->Result<char, ()> {Ok(x)});
            let metrics = source_span::DEFAULT_METRICS;
            let buffer = SourceBuffer::new(chars, source_span::Position::default(), metrics);
            let mut form = Formatter::new();

            buffer.iter().for_each(|_|()); 
            for i in v { 
                let s = InfileLoc::from_loc(i.get_loc(), &file);
                form.add(s.span.clone(), Some(Self::generate_error_message(i)), source_span::fmt::Style::Error);
            }
            let t = form.render(buffer.iter(), buffer.span().clone(), &metrics);
            println!("{}", t.unwrap())
        }
        Ok(!self.errors.is_empty())
    }
}