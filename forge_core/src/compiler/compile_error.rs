use std::fmt::{Display, Formatter};
use crate::parser::ParseError;

#[derive(Eq, Clone, PartialEq, Debug)]
pub struct CompileError(pub usize, pub usize, pub String);

impl From<ParseError> for CompileError {
    fn from(value: ParseError) -> Self {
        Self(value.0, value.1, value.2)
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{}) {}", self.0, self.1, self.2)
    }
}
