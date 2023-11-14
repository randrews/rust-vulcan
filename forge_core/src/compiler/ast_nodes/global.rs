use crate::ast::{Global, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;
use crate::compiler::utils::Variable;

impl Compilable for Global {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, _loc: Location) -> Result<(), CompileError> {
        if self.initial.is_some() {
            todo!("Globals with initials are not yet supported")
        }
        state.add_global(&self.name, |s| Variable::IndirectLabel(s.gensym()))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_global_decl() {
        let mut state = State::default();
        parse("global a;")
            .unwrap()
            .process(&mut state, None, (0, 0).into())
            .unwrap();
        assert_eq!(
            state.global_scope,
            [("a".into(), Variable::IndirectLabel("_forge_gensym_1".into()))].into()
        )
    }

    #[test]
    fn test_name_collision() {
        let mut state = State::default();
        assert!(parse("const a = 7; global a;")
            .unwrap()
            .process(&mut state, None, (0, 0).into())
            .is_err());
    }
}