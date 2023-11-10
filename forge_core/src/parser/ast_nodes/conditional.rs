use crate::ast::{Block, Conditional, Expr};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Conditional {
    const RULE: PestRule = PestRule::conditional;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let condition = Expr::from_pair(inner.next().unwrap());
        let body = Block::from_pair(inner.next().unwrap());
        let alternative = inner.next().map(Block::from_pair);
        Self {
            condition,
            body,
            alternative,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::parser::Parseable;
    use crate::parser::ast_nodes::test_utils::*;
    use super::*;

    #[test]
    fn parse_conditional() {
        if let Ok(Statement::Conditional(Conditional { condition, body, alternative })) =
            Statement::from_str("if(cond) { foo(); }") {
            assert_eq!(condition, Expr::from_str("cond").unwrap());
            assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
            assert_eq!(alternative, None);
        } else {
            panic!()
        }
    }

    #[test]
    fn parse_else_conditional() {
        if let Ok(Statement::Conditional(Conditional { condition, body, alternative })) =
            Statement::from_str("if(cond) { foo(); } else { bar(); }") {
            assert_eq!(condition, Expr::from_str("cond").unwrap());
            assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
            assert_eq!(alternative.unwrap().0[0].ast, Block::from_str("{ bar(); }").unwrap().0[0].ast);
        } else {
            panic!()
        }
    }
}