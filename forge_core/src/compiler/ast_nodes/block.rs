use crate::ast::{Asm, Block, Location, Statement};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Block {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, _loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Block outside function");

        let frame_size_before_block = sig.frame_size();

        // Compile each statement:
        for stmt in self.0 {
            let loc = stmt.location;
            sig.emit_comment(format!(";; {}:{} :: {}", loc.line, loc.col, stmt.ast.description()));
            match stmt.ast {
                Statement::Break => {
                    if sig.in_loop() {
                        sig.emit("#break")
                    } else {
                        return Err(CompileError(loc.line, loc.col, "Break outside loop".to_string()))
                    }
                }
                Statement::Continue => {
                    if sig.in_loop() {
                        // This is subtle. To "continue" a while or until loop, we simply need to #continue
                        // and the assembler takes care of the macro for us. Repeat loops are different though
                        // because the counter inc / dec takes place at the end of the loop, so we can't use
                        // the macro, we just want to generate a jmpr instead
                        if let Some(label) = sig.continue_point() {
                            sig.emit_arg("jmpr", format!("@{}", label))
                        } else {
                            sig.emit("#continue")
                        }
                    } else {
                        return Err(CompileError(loc.line, loc.col, "Continue outside loop".to_string()))
                    }
                }
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
                Statement::Conditional(cond) => {
                    cond.process(state, Some(sig), loc)?;
                }
                Statement::WhileLoop(while_loop) => {
                    while_loop.process(state, Some(sig), loc)?;
                }
                Statement::RepeatLoop(repeat_loop) => {
                    repeat_loop.process(state, Some(sig), loc)?;
                }
                Statement::Once(once) => {
                    once.process(state, Some(sig), loc)?;
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
    use crate::compiler::CompileError;
    use crate::compiler::test_utils::*;

    #[test]
    fn test_asm_statements() {
        assert_eq!(
            test_body(state_for("fn test() { var x; asm(&x) { swap 12\nstorew } }")),
            vec![
                "peekr", // Push the addr of x
                "swap 12", // The asm body, which swaps 12 behind it and stores it there
                "storew",
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_break_statements() {
        assert_eq!(
            test_body(state_for("fn test() { repeat(10) { break; } }")),
            vec![
                "push 10", "#while", "dup", "agt 0", "#do", // standard repeat loop header
                "#break", // the break
                "_forge_gensym_3:", "sub 1", "#end", "pop" // repeat loop footer
            ].join("\n")
        );

        assert_eq!(
            test_body(state_for("fn test() { while(1) { break; } }")),
            vec![
                "#while", "push 1", "#do", // standard repeat loop header
                "#break", // the break
                "#end" // repeat loop footer
            ].join("\n")
        )
    }

    #[test]
    fn test_continue_statements() {
        assert_eq!(
            test_body(state_for("fn test() { repeat(10) { continue; } }")),
            vec![
                "push 10", "#while", "dup", "agt 0", "#do", // standard repeat loop header
                "jmpr @_forge_gensym_3", // the continue, jumping to the label
                "_forge_gensym_3:", // The label demnoting the start of the counter update
                "sub 1", "#end", "pop" // repeat loop footer
            ].join("\n")
        );
    }

    #[test]
    fn test_malformed_break_continue_statements() {
        assert_eq!(
            test_err("fn test() { break; }"),
            Some(CompileError(1, 13, "Break outside loop".to_string()))
        );
        assert_eq!(
            test_err("fn test() { continue; }"),
            Some(CompileError(1, 13, "Continue outside loop".to_string()))
        );
    }

    #[test]
    fn test_block_scoping() {
        assert_eq!(
            test_body(state_for("fn test() { repeat(5) c { 2; } var k = 5; return k; }")),
            vec![
                "push 0", // Create the 'c' var and store 0 in it
                "peekr",
                "storew",
                "push 5",
                "#while", // Starting the loop:
                "dup", // Copy the limit
                "peekr", // Load c
                "loadw",
                "sub", // Subtract c from a
                "agt 0", // Are we still positive?
                "#do",
                "push 2", // Pointless loop body
                "pop",
                "_forge_gensym_3:",
                "peekr", // Load c as an lvalue
                "dup", // Dup it, load it, add 1
                "loadw",
                "add 1",
                "swap", // Swap the addr on top and store it
                "storew",
                "#end", // End of the loop body!
                "pop", // Drop the limit off the top
                "push 5", // Push the rvalue we'll put in 'k'
                "peekr", // THIS IS THE TEST: 'k' should go at frame + 0, because it's
                "storew", // taking the same (now freed) frame slot that c took, because c is
                "peekr", // now out of scope
                "loadw",
                "jmpr @_forge_gensym_2",
            ]
                .join("\n")
        );
    }
}