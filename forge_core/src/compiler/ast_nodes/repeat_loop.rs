use crate::ast::{Expr, Location, RepeatLoop, VarDecl};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for RepeatLoop {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Repeat loop outside function");

        let RepeatLoop { count, name, body } = self;
        let named_counter = name.is_some();
        let counter_name = name.unwrap_or_default();

        if named_counter {
            let decl = VarDecl {
                name: counter_name.clone(),
                size: None,
                initial: Some(Expr::Number(0)),
            };
            decl.process(state, Some(sig), loc)?;
        }

        // Okay, the counter (if present) is now declared and in scope; we'll eval the limit once
        count.process(state, Some(sig), loc)?;

        // Now we have a fairly normal while loop:
        sig.emit("#while");
        // Stack currently has the limit on top. We need to cmp the counter to that.
        sig.emit("dup");

        // Load the counter, if present:
        if named_counter {
            Expr::Name(counter_name.clone()).process(state, Some(sig), loc)?;
            // Subtract the counter from (the copy of) the limit.
            sig.emit("sub");
        }

        // cmp that to zero, for our flag: this is either limit - ctr or limit, depending if
        // there's a counter
        sig.emit_arg("agt", 0);

        // Loop body:
        sig.emit("#do");
        body.process(state, Some(sig), loc)?;

        // After the body we need to increment the counter if there is one
        if named_counter {
            // Put its address on top:
            Expr::Address(Expr::Name(counter_name.clone()).into()).process(state, Some(sig), loc)?;
            // Dup and load it:
            sig.emit("dup");
            sig.emit("loadw");
            // Increment it:
            sig.emit_arg("add", 1);
            // Now we have ( counted-addr new-ctr-val ) so swap and store
            sig.emit("swap");
            sig.emit("storew");
        } else {
            // No counter, so just decrement the limit, which is already on top:
            sig.emit_arg("sub", 1);
        }

        // Back to the check!
        sig.emit("#end");
        // Done with the loop, but the limit is still on the stack, pop it:
        sig.emit("pop");

        // If we have added a counter variable, we need to forget that because (even
        // though declared outside the block) it should be scoped to the block:
        if named_counter {
            sig.forget_last_local();
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_named_repeat_loops() {
        // With a counter
        assert_eq!(
            test_body(state_for("fn test(a) { var x = 0; repeat(a) c { x = x + c; } return x; }")),
            vec![
                "loadw frame", // capture arg a
                "storew",
                "push 0", // Create the 'x' var and store 0 in it
                "loadw frame",
                "add 3",
                "storew",
                "push 0", // Create the 'c' var and store 0 in it
                "loadw frame",
                "add 6",
                "storew",
                "loadw frame", // Load 'a' from args
                "loadw",
                "#while", // Starting the loop:
                "dup", // Copy the limit
                "loadw frame", // Load c
                "add 6",
                "loadw",
                "sub", // Subtract c from a
                "agt 0", // Are we still positive?
                "#do",
                "loadw frame", // Load x
                "add 3",
                "loadw",
                "loadw frame", // Load c
                "add 6",
                "loadw",
                "add", // Add c to x
                "loadw frame", // Load x as an lvalue
                "add 3",
                "storew", // Store c + x into it
                "loadw frame", // Load c as an lvalue
                "add 6",
                "dup", // Dup it, load it, add 1
                "loadw",
                "add 1",
                "swap", // Swap the addr on top and store it
                "storew",
                "#end", // End of the loop body!
                "pop", // Drop the limit off the top
                "loadw frame", // Load x so we can return it
                "add 3",
                "loadw",
                "ret",
            ]
                .join("\n")
        );
    }

    #[test]
    fn test_anon_repeat_loops() {
        // No counter
        assert_eq!(
            test_body(state_for("fn test(a) { var x = 1; repeat(a) { x = x * 2; } return x; }")),
            vec![
                "loadw frame", // capture arg a
                "storew",
                "push 1", // Create the 'x' var and store 1 in it
                "loadw frame",
                "add 3",
                "storew",
                "loadw frame", // Load 'a' from args
                "loadw",
                "#while", // Starting the loop:
                "dup", // Copy the limit
                "agt 0", // Are we still positive?
                "#do",
                "loadw frame", // Load x
                "add 3",
                "loadw",
                "push 2", // double it
                "mul",
                "loadw frame", // Load x as an lvalue
                "add 3",
                "storew", // Store 2x into it
                "sub 1", // decrement the counter
                "#end", // End of the loop body!
                "pop", // Drop the limit off the top
                "loadw frame", // Load x so we can return it
                "add 3",
                "loadw",
                "ret",
            ].join("\n")
        );
    }
}