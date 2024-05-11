use crate::ast::{Block, Once};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Once {
    const RULE: PestRule = PestRule::once;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner().peekable();
        let body = Block::from_pair(inner.next().unwrap());
        Self { body }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::parser::test_utils::{dislocate, dislocated_block};
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_once_blocks() {
        assert!(match Statement::from_str("once { foo(x); }") {
            Ok(Statement::Once(Once { body })) => {
                assert_eq!(dislocate(body.0), dislocated_block("{ foo(x); }"));
                true
            },
            _ => false
        });
    }
}