use crate::ast::FunctionPrototype;
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for FunctionPrototype {
    const RULE: PestRule = PestRule::function_prototype;
    fn from_pair(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let args: Vec<_> = inner
            .next()
            .unwrap()
            .into_inner()
            .map(|p| String::from(p.as_str()))
            .collect();

        Self { name, args }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_fn_prototype() {
        assert_eq!(
            FunctionPrototype::from_str("fn foo();"),
            Ok(FunctionPrototype {
                name: "foo".into(),
                args: vec![],
            })
        );
    }
}