use pest::error::{Error, LineColLocation};
use crate::parser::PestRule;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ParseError(pub usize, pub usize, pub String);

impl From<Error<PestRule>> for ParseError {
    fn from(err: Error<PestRule>) -> Self {
        let message = err.to_string();
        match err.line_col {
            LineColLocation::Pos((line, col)) | LineColLocation::Span((line, col), _) => {
                Self(line, col, message)
            }
        }
    }
}