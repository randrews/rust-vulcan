use crate::ast::{Location, Return};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Return {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Block outside function");

        match self {
            Return(None) => {
                // Returning nothing, so just default to returning a 0:
                sig.emit_arg("push", 0);
            }
            Return(Some(expr)) => {
                // Eval the expr and emit a ret for it
                expr.process(state, Some(sig), loc)?;
            }
        }

        // The return value is on the stack so jmpr to the outro to do the actual return
        sig.emit_arg("jmpr", format!("@{}",sig.end_label));

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_returns_with_value() {
        assert_eq!(
            test_body(state_for("fn test(a) { return a + 3; }")),
            vec![
                "peekr", // Load a
                "loadw",
                "push 3", // Add 3
                "add",
                "jmpr @_forge_gensym_2", // Return that
            ]
                .join("\n")
        );
    }

    #[test]
    fn test_void_returns() {
        assert_eq!(
            test_body(state_for("fn test(a) { if (a > 0) { return; } }")),
            vec![
                "peekr", // Load a
                "loadw",
                "push 0", // Compare to 0
                "agt",
                "#if", // If statement
                "push 0", // Default return value, for an expr-less return
                "jmpr @_forge_gensym_2",
                "#end",
            ]
                .join("\n")
        );
    }
}