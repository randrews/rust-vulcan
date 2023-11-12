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

        // todo this makes arrays not work. we can no longer statically compute the frame size, we
        // need to have the frame size stored at runtime somehow, probably rstack. This will mean
        // every fn will stick the frame ptr it's given in the rstack in its preamble, and that any
        // vardecl will increment that ptr (that the top of the rstack is the end of the current
        // frame, essentially). Returns also need to popr/pop before doing the actual ret.
        // todo also for that matter, that means that the frame ptr no longer needs to be a global
        // symbol: have it be stored in the rstack and passed as the first param in a call. So the
        // calling convention is, push the args, push the new fn's frame ptr (current top-of-frame+1),
        // call. The receiving fn puts its frame ptr into the rstack, dups it there, puts the args
        // (it knows its own arity) into its frame, and then every vardecl grabs the 2nd thing from r,
        // increments it by whatever, and puts it back.
        // todo in fact, a larger refactor would just store the frame size as the first thing in the
        // frame, to make vardecls faster

        // Do the call:
        sig.emit("call");

        // And this is where we'll return to. The function has popped its args and left a word on
        // the stack as a return value, which someone else will deal with (this is the word that
        // this expr::call will end up evaluating to). But before we're done, we need to restore
        // our frame pointer:
        //sig.emit("popr");
        //sig.emit("storew frame");

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
            ]
                .join("\n")
        );
    }
}