use crate::ast::{Call, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Call {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        // Require a function
        let sig = sig.expect("lvalue outside a function");

        // We need a stack consisting of the arguments (last on top), followed by the new fn's frame
        // pointer, followed by the address to call.
        // So, first eval the args:
        for arg in self.args {
            arg.process(state, Some(sig), loc)?;
        }

        // Now we increment the frame ptr to right after the current frame:
        // todo this messes with recursive calls: we're hard-coding the frame size _right now,_
        // which may increase later with later vardecls in the fn, which could be _executed_ before
        // this call is. We need to store the top of the frame at runtime somehow.
        sig.emit("peekr");
        let frame_size = sig.frame_size();
        if frame_size > 0 {
            sig.emit_arg("add", frame_size);
        }

        // Eval the target
        self.target.0.process(state, Some(sig), loc)?;

        // Before we actually do the call though, we need to deal with some paperwork around the
        // frame pointer. We will store the current frame pointer in the rstack:
        //sig.emit("loadw frame");
        //sig.emit("pushr");

        // Do the call:
        sig.emit("call");

        // And this is where we'll return to. The function has popped its args and left a word on
        // the stack as a return value, which someone else will deal with (this is the word that
        // this expr::call will end up evaluating to). What's left on our rstack now is our frame ptr,
        // which is what we want.

        // And we're finished!
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_calls() {
        assert_eq!(
            test_body(state_for("fn test(a, b) { test(2, 3); }")),
            vec![
                "pushr",
                "peekr", // capture arg b
                "add 3",
                "storew",
                "peekr", // capture arg a
                "storew",
                "push 2", // evaluating args, in order
                "push 3",
                "peekr", // The new fn's frame ptr:
                "add 6",
                "push _forge_gensym_1", // evaluating target (this fn)
                "call", // Actually make the call
                "pop", // expr-as-statement drops the evaluated value
                "popr", "pop", "ret 0" // Implicit void return
            ]
                .join("\n")
        );
    }
}