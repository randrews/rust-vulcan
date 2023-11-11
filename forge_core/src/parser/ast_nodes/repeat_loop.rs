use crate::ast::{Block, Expr, RepeatLoop};
use crate::parser::{AstNode, Pair, PairsExt, PestRule};

impl AstNode for RepeatLoop {
    const RULE: PestRule = PestRule::repeat_loop;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner().peekable();
        let count = Expr::from_pair(inner.next().unwrap());
        let name = inner
            .next_if_rule(PestRule::name)
            .map(|p| String::from(p.as_str()));
        let body = Block::from_pair(inner.next().unwrap());
        Self { count, name, body }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::parser::test_utils::{dislocate, dislocated_block};
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_repeat_loops_with_name() {
        assert!(match Statement::from_str("repeat(10) x { foo(x); }") {
            Ok(Statement::RepeatLoop(RepeatLoop { count, name, body })) => {
                assert_eq!(count, 10.into());
                assert_eq!(name, Some("x".into()));
                assert_eq!(dislocate(body.0), dislocated_block("{ foo(x); }"));
                true
            },
            _ => false
        });
    }

    #[test]
    fn parse_anon_repeat_loops() {
        assert!(match Statement::from_str("repeat(10) { foo(); }") {
            Ok(Statement::RepeatLoop(RepeatLoop { count, name, body })) => {
                assert_eq!(count, 10.into());
                assert_eq!(name, None);
                assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
                true
            },
            _ => false
        });
    }
}