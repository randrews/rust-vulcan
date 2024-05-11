use crate::ast::{Once, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Once {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Once block outside function");

        let Once { body } = self;
        let sym = state.add_flag();

        sig.emit_arg("loadw", sym.clone());
        sig.emit("#if");
        sig.emit_arg("push", 0);
        sig.emit_arg("storew", sym);
        body.process(state, Some(sig), loc)?;
        sig.emit("#end");
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_once() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 0; once { x = 1; } }")),
            vec![
                "push 0",
                "peekr",
                "storew", // x = 0
                "loadw _forge_gensym_3", // Load the once flag
                "#if",
                "push 0", // clear the once flag
                "storew _forge_gensym_3",
                "push 1", // x = 1
                "peekr",
                "storew",
                "#end" // end once block
            ]
                .join("\n")
        );
    }
}