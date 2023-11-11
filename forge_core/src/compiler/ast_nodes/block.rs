use crate::ast::{Asm, Block, Conditional, Location, Statement, WhileLoop};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Block {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, _loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Block outside function");

        let frame_size_before_block = sig.frame_size;

        // Compile each statement:
        for stmt in self.0 {
            let loc = stmt.location;
            match stmt.ast {
                Statement::Return(ret) => {
                    ret.process(state, Some(sig), loc)?;
                }
                Statement::Assignment(assign) => assign.process(state, Some(sig), loc)?,
                Statement::Expr(expr) => {
                    expr.process(state, Some(sig), loc)?;
                    // Every expr leaves a single-word return value on the stack. In an rvalue this
                    // is useful but in a statement it's garbage (because nothing else is about to
                    // pick it up) so, drop it:
                    sig.emit("pop")
                }
                Statement::VarDecl(vardecl) => vardecl.process(state, Some(sig), loc)?,
                Statement::Asm(Asm { args, body }) => {
                    // Process all the args, if any
                    for a in args {
                        (*a.0).process(state, Some(sig), loc)?
                    }
                    // Emit the body
                    sig.emit(body.as_str())
                }
                Statement::Conditional(Conditional { condition, body, alternative }) => {
                    // todo split into new file
                    condition.process(state, Some(sig), loc)?;
                    sig.emit("#if"); // We went to a lot of trouble making macros, shame not to use them
                    body.process(state, Some(sig), loc)?;
                    if let Some(alternative) = alternative {
                        sig.emit("#else");
                        alternative.process(state, Some(sig), loc)?;
                    }
                    sig.emit("#end")
                }
                Statement::WhileLoop(WhileLoop { condition, body }) => {
                    // todo split into new file
                    sig.emit("#while");
                    condition.process(state, Some(sig), loc)?;
                    sig.emit("#do");
                    body.process(state, Some(sig), loc)?;
                    sig.emit("#end")
                }
                Statement::RepeatLoop(repeat_loop) => {
                    repeat_loop.process(state, Some(sig), loc)?;
                }
            }
        }

        // If we declared anything inside the block, we'll know that because the frame size will
        // have increased, so, simply blow away those names to make them block scoped. This doesn't
        // give us shadowing, it's not a true scope chain, but this is much simpler and fixes some
        // common cases where you want that.
        sig.reduce_frame_size_to(frame_size_before_block);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_asm_statements() {
        assert_eq!(
            test_body(state_for("fn test() { var x; asm(&x) { swap 12\nstorew } }")),
            vec![
                "loadw frame", // Push the addr of x
                "swap 12", // The asm body, which swaps 12 behind it and stores it there
                "storew",
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_conditionals() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 3; if (x > 2) { x = 1; } }")),
            vec![
                "push 3",
                "loadw frame",
                "storew", // x = 3
                "loadw frame",
                "loadw",
                "push 2",
                "agt", // The condition, x > 2
                "#if", // The if itself
                "push 1",
                "loadw frame",
                "storew", // The branch, x = 1
                "#end"]
                .join("\n")
        );

        assert_eq!(
            test_body(state_for("fn test() { var x = 3; if (x > 2) { x = 1; } else { x = 7; } }")),
            vec![
                "push 3",
                "loadw frame",
                "storew", // x = 3
                "loadw frame",
                "loadw",
                "push 2",
                "agt", // The condition, x > 2
                "#if", // The if itself
                "push 1",
                "loadw frame",
                "storew", // The affirmative branch, x = 1
                "#else", // The alternative
                "push 7",
                "loadw frame",
                "storew", // x = 7
                "#end"]
                .join("\n")
        )
    }

    #[test]
    fn test_while_loops() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 0; var c = 0; while (c < 10) { x = x + c; c = c + 1; } }")),
            vec![
                "push 0",
                "loadw frame",
                "storew", // var x = 0
                "push 0",
                "loadw frame",
                "add 3",
                "storew", // var c = 0
                "#while", // Start the loop
                "loadw frame",
                "add 3",
                "loadw",
                "push 10",
                "alt", // c > 10
                "#do", // Start loop body
                "loadw frame",
                "loadw",
                "loadw frame",
                "add 3",
                "loadw",
                "add",
                "loadw frame",
                "storew", // x = x + c
                "loadw frame",
                "add 3",
                "loadw",
                "push 1",
                "add",
                "loadw frame",
                "add 3",
                "storew", // c = c + 1
                "#end", // End the loop body
            ]
                .join("\n")
        );
    }

    #[test]
    fn test_block_scoping() {
        assert_eq!(
            test_body(state_for("fn test() { repeat(5) c { 2; } var k = 5; return k; }")),
            vec![
                "push 0", // Create the 'c' var and store 0 in it
                "loadw frame",
                "storew",
                "push 5",
                "#while", // Starting the loop:
                "dup", // Copy the limit
                "loadw frame", // Load c
                "loadw",
                "sub", // Subtract c from a
                "agt 0", // Are we still positive?
                "#do",
                "push 2", // Pointless loop body
                "pop",
                "loadw frame", // Load c as an lvalue
                "dup", // Dup it, load it, add 1
                "loadw",
                "add 1",
                "swap", // Swap the addr on top and store it
                "storew",
                "#end", // End of the loop body!
                "pop", // Drop the limit off the top
                "push 5", // Push the rvalue we'll put in 'k'
                "loadw frame", // THIS IS THE TEST: 'k' should go at frame + 0, because it's
                "storew", // taking the same (now freed) frame slot that c took, because c is
                "loadw frame", // now out of scope
                "loadw",
                "ret",
            ]
                .join("\n")
        );
    }
}