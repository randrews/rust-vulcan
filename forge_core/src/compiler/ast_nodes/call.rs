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

        // Whatever shall we send as a frame pointer to the new fn? Our pool pointer, because we
        // know that's free, even if we've alloced anything, it'll increment it:
        sig.emit("popr"); sig.emit("peekr"); // take the frame ptr off and copy the pool off
        sig.emit("swap"); sig.emit("pushr"); // swap the pool under the frame and put the frame back on

        // Eval the target
        self.target.0.process(state, Some(sig), loc)?;

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
                "push 2", // evaluating args, in order
                "push 3",
                "popr", "peekr", "swap", "pushr", // Pool ptr to send to the new call
                "push _forge_gensym_1", // evaluating target (this fn)
                "call", // Actually make the call
                "pop", // expr-as-statement drops the evaluated value
            ]
                .join("\n")
        );
    }
}