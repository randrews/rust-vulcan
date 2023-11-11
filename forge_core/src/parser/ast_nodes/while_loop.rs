use crate::ast::{Block, Expr, WhileLoop};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for WhileLoop {
    const RULE: PestRule = PestRule::while_loop;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        Self {
            condition: Expr::from_pair(inner.next().unwrap()),
            body: Block::from_pair(inner.next().unwrap()),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::parser::test_utils::{dislocate, dislocated_block};
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_while_loops() {
        assert!(match Statement::from_str("while(cond) { foo(); }") {
            Ok(Statement::WhileLoop(WhileLoop { condition, body })) => {
                assert_eq!(condition, Expr::from_str("cond").unwrap());
                assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
                true
            }
            _ => false
        });
    }
}