use crate::ast::{Asm, Expr};
use crate::parser::{AstNode, PestRule, Pair};

impl AstNode for Asm {
    const RULE: PestRule = PestRule::asm;
    fn from_pair(pair: Pair) -> Self {
        let mut args = Vec::new();
        let mut body = None;
        for p in pair.into_inner() {
            match p.as_rule() {
                PestRule::expr => args.push(Expr::from_pair(p).into()),
                PestRule::asm_body => body = Some(String::from(p.as_str().trim())),
                _ => unreachable!()
            }
        }
        Self { args, body: body.unwrap() }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_asm() {
        assert_eq!(
            Asm::from_str("asm { push 34 }"),
            Ok(Asm { args: vec![], body: "push 34".into() })
        );

        assert_eq!(
            Asm::from_str("asm (&a) { swap 34\nstorew }"),
            Ok(Asm { args: vec![Expr::Address("a".into()).into()], body: "swap 34\nstorew".into() })
        );
    }
}