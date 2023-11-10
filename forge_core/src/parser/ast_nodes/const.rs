use crate::ast::{Const, Expr};
use crate::parser::{AstNode, Pair, PairExt, PestRule};

impl AstNode for Const {
    const RULE: PestRule = PestRule::const_decl;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let value = inner.next().unwrap();
        match value.as_rule() {
            PestRule::string => Const {
                name,
                string: Some(value.into_quoted_string()),
                value: None,
            },
            PestRule::expr => Const {
                name,
                string: None,
                value: Some(Expr::from_pair(value)),
            },
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_decimal_consts() {
        assert_eq!(
            Const::from_str("const a = 123;"),
            Ok(Const {
                name: "a".into(),
                value: Some(123.into()),
                string: None,
            })
        )
    }

    #[test]
    fn parse_hex() {
        assert_eq!(
            Const::from_str("const a = 0xaa;"),
            Ok(Const {
                name: "a".into(),
                value: Some(0xaa.into()),
                string: None,
            })
        )
    }

    #[test]
    fn parse_negative() {
        assert_eq!(
            Const::from_str("const a = -7;"),
            Ok(Const {
                name: "a".into(),
                value: Some(Expr::Neg(7.into()).into()),
                string: None,
            })
        )
    }

    #[test]
    fn parse_string() {
        assert_eq!(
            Const::from_str("const a = \"foo bar\";"),
            Ok(Const {
                name: "a".into(),
                value: None,
                string: Some("foo bar".into()),
            })
        )
    }
}