use crate::ast::{Block, Function};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Function {
    const RULE: PestRule = PestRule::function;
    fn from_pair(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let args: Vec<_> = inner
            .next()
            .unwrap()
            .into_inner()
            .map(|p| String::from(p.as_str()))
            .collect();

        let body = Block::from_pair(inner.next().unwrap());

        Self { name, args, body }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_function_headers() {
        assert_eq!(
            Function::from_str("fn foo() {}"),
            Ok(Function {
                name: "foo".into(),
                args: vec![],
                body: Block(vec![]),
            })
        )
    }

    #[test]
    fn parse_fns_with_args() {
        assert_eq!(
            Function::from_str("fn foo(a, b) {}"),
            Ok(Function {
                name: "foo".into(),
                args: vec!["a".into(), "b".into()],
                body: Block(vec![]),
            })
        );
    }
}