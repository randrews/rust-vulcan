use crate::ast::{Conditional, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Conditional {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Conditional outside function");

        let Conditional { condition, body, alternative } = self;

        condition.process(state, Some(sig), loc)?;
        sig.emit("#if"); // We went to a lot of trouble making macros, shame not to use them
        body.process(state, Some(sig), loc)?;
        if let Some(alternative) = alternative {
            sig.emit("#else");
            alternative.process(state, Some(sig), loc)?;
        }
        sig.emit("#end");
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_conditionals() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 3; if (x > 2) { x = 1; } }")),
            vec![
                "pushr",
                "push 3",
                "peekr",
                "storew", // x = 3
                "peekr",
                "loadw",
                "push 2",
                "agt", // The condition, x > 2
                "#if", // The if itself
                "push 1",
                "peekr",
                "storew", // The branch, x = 1
                "#end",
                "popr", "pop", "ret 0" // Implicit void return
            ]
                .join("\n")
        );
    }

    #[test]
    fn test_if_else() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 3; if (x > 2) { x = 1; } else { x = 7; } }")),
            vec![
                "pushr",
                "push 3",
                "peekr",
                "storew", // x = 3
                "peekr",
                "loadw",
                "push 2",
                "agt", // The condition, x > 2
                "#if", // The if itself
                "push 1",
                "peekr",
                "storew", // The affirmative branch, x = 1
                "#else", // The alternative
                "push 7",
                "peekr",
                "storew", // x = 7
                "#end",
                "popr", "pop", "ret 0" // Implicit void return
            ]
                .join("\n")
        )
    }
}