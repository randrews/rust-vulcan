use crate::ast::{Block, Statement};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Block {
    const RULE: PestRule = PestRule::block;

    fn from_pair(pair: Pair) -> Self {
        Self(pair.into_inner().map(Statement::from_pair_located).collect())
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{Call, Expr};
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_block() {
        let block = Block::from_str("{ foo(); bar(); }").unwrap();
        let statements: Vec<_> = block.0.into_iter().map(|s| s.ast).collect();
        assert_eq!(
            statements,
            vec![
                Statement::Expr(Expr::Call(Call { target: "foo".into(), args: vec![] })),
                Statement::Expr(Expr::Call(Call { target: "bar".into(), args: vec![] })),
            ]
        );
    }
}