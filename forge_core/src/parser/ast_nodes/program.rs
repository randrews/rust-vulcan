use crate::ast::{Declaration, Program};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Program {
    const RULE: PestRule = PestRule::program;
    fn from_pair(pair: Pair) -> Self {
        // Program captures EOI, to make sure that it's parsing the entire stream. We need to
        // ignore that though:
        Self(
            pair.into_inner()
                .filter(|p| p.as_rule() != PestRule::EOI)
                .map(Declaration::from_pair_located)
                .collect(),
        )
    }
}

#[cfg(test)]
mod test {
    use crate::parser::ast_nodes::test_utils::dislocate;
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_program() {
        let prog = Program::from_str("global foo; const blah = 3;").unwrap();
        let decls: Vec<_> = dislocate(prog.0);
        assert_eq!(
            decls,
            vec![
                Declaration::from_str("global foo;").unwrap(),
                Declaration::from_str("const blah = 3;").unwrap(),
            ]
        )
    }
}