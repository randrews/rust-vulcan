use crate::ast::{Expr, Global};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Global {
    const RULE: PestRule = PestRule::global;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let size = inner.next().map(Expr::from_pair);

        Global { name, size }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_globals() {
        assert_eq!(
            Global::from_str("global foo;"),
            Ok(Global {
                name: "foo".into(),
                size: None,
            })
        )
    }

    #[test]
    fn parse_global_arrays() {
        assert_eq!(
            Global::from_str("global foo[10];"),
            Ok(Global {
                name: "foo".into(),
                size: Some(10.into()),
            })
        );
    }
}