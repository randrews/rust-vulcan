use crate::ast::{Global, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;
use crate::compiler::utils::Variable;

impl Compilable for Global {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let label = state.gensym();
        if let Some(expr) = self.initial {
            // Let's make a fake CompiledFn to compile this expr in:
            let mut f = CompiledFn::default();
            // Process the initial value and add that to the state's init
            expr.process(state, Some(&mut f), loc)?;
            state.init.append(&mut f.body);
            // Now actually store it:
            state.init.emit_arg("storew", label.clone());
        }

        state.add_global(&self.name, |_| Variable::IndirectLabel(label.clone()))
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::state_for;
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

    #[test]
    fn test_init() {
        let state = state_for("global a = 5 + 3;");
        assert_eq!(
            state.init.0.join("\n"),
            vec![
                "push 8",
                "storew _forge_gensym_1"
            ].join("\n")
        )
    }
}