use source_span::{Position, SourceBuffer, Span};

use crate::{ast::Loc, compiler::FileId};




pub struct InfileLoc {
    pub span: Span,
}

impl InfileLoc {
    pub fn from_loc(loc: Loc, file_content: &str) -> Self {
        let metrics = &source_span::DEFAULT_METRICS; // characters metrics
        let mut pos = Position::new(0, 0); 
        let mut span = Span::default();
        for (i, c) in file_content.char_indices() {
            println!("{i}: {}, {pos}, {span}", c as u32);
            if i < loc.begin {
                pos.shift(c, metrics)
            } else if i == loc.begin {
                span = pos.into();
            } else if i <= loc.end {
                span.push(c, metrics)
                
            }
        }
        SourceBuffer::new(file_content.chars().map(|x|-> Result<char, ()> {Result::Ok(x)}), Default::default(), metrics.clone());
        InfileLoc { span }
        
    }
}
pub struct ErrorDisplay {

}
