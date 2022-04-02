use std::fmt::{Display, Formatter};

use vcore::opcodes::InvalidMnemonic;

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError<'a> {
    LineParseFailure,
    InvalidInstruction(&'a str),
}

impl<'a> From<InvalidMnemonic<'a>> for ParseError<'a> {
    fn from(err: InvalidMnemonic<'a>) -> Self {
        Self::InvalidInstruction(err.0)
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ParseError::*;
        match self {
            LineParseFailure => write!(f, "Failed to parse line"),
            InvalidInstruction(p) => write!(f, "Cannot parse {} as instruction", p),
        }
    }
}
