use crate::ast::{Block, Located, Statement};
use crate::parser::Parseable;

pub fn dislocate<T>(block: Vec<Located<T>>) -> Vec<T> {
    block.into_iter().map(|l| l.ast).collect()
}

pub fn dislocated_block(src: &str) -> Vec<Statement> {
    dislocate(Block::from_str(src).unwrap().0)
}
