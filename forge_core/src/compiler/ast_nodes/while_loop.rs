use crate::ast::{Location, WhileLoop};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for WhileLoop {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Loop outside function");
        let WhileLoop { condition, body } = self;
        sig.emit("#while");
        condition.process(state, Some(sig), loc)?;
        sig.emit("#do");
        body.process(state, Some(sig), loc)?;
        sig.emit("#end");

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_while_loops() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 0; var c = 0; while (c < 10) { x = x + c; c = c + 1; } }")),
            vec![
                "pushr",
                "push 0",
                "peekr",
                "storew", // var x = 0
                "push 0",
                "peekr",
                "add 3",
                "storew", // var c = 0
                "#while", // Start the loop
                "peekr",
                "add 3",
                "loadw",
                "push 10",
                "alt", // c > 10
                "#do", // Start loop body
                "peekr",
                "loadw",
                "peekr",
                "add 3",
                "loadw",
                "add",
                "peekr",
                "storew", // x = x + c
                "peekr",
                "add 3",
                "loadw",
                "push 1",
                "add",
                "peekr",
                "add 3",
                "storew", // c = c + 1
                "#end" // End the loop body
            ]
                .join("\n")
        );
    }

}