use std::fmt::{Display, Formatter};

use vcore::opcodes::InvalidMnemonic;

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    LineParseFailure,
    InvalidInstruction(String),
}

impl<'a> From<InvalidMnemonic<'a>> for ParseError {
    fn from(err: InvalidMnemonic<'a>) -> Self {
        Self::InvalidInstruction(err.0.into())
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ParseError::*;
        match self {
            LineParseFailure => write!(f, "Failed to parse line"),
            InvalidInstruction(p) => write!(f, "Cannot parse {} as instruction", p),
        }
    }
}
