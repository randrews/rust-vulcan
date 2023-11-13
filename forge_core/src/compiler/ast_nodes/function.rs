use crate::ast::{Function, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Function {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let label = state.find_or_declare_function(self.name.as_str(), loc)?;
        let end_label = state.gensym(); // The label of the outro of the fn

        // The CompiledFn for this function, which will eventually get stuff populated into it:
        let mut sig = CompiledFn {
            label,
            end_label,
            ..Default::default()
        };

        // todo we need to store arity somehow in the state, so we can check arglists even
        // with recursive calls. This probably becomes splitting "signature" from "function context"
        // and setting signature immutably right now, passing context down ("block?") and setting it
        // at the end

        // Some stuff needs to happen before we can compile the body, we need to at least know how
        // many args there are, so they're accounted in the frame size:
        sig.calculate_arity(&self.args)?;

        // Compile the body, storing all of it in the CompiledFn we just created / added
        self.body.process(state, Some(&mut sig), loc)?;

        // This generates the code to copy the args into the frame as well as adds all the arguments
        // to the local scope and deals with the pool pointer. We can't run this until after the
        // body is compiled (won't know if we have any args / need to mess with pool) but things
        // created here will be emitted before (preamble) and after (outro) the body
        sig.generate_preamble_outro(&self.args)?;

        // This can't fail because if it were a dupe name, adding the global would have failed
        state.functions.insert(self.name.clone(), sig);

        // And we're done: the function body now stores everything we need to emit that function...
        // but we can't actually emit it yet because we don't know how we're building things. This
        // is the equivalent of the object-file step in a more real compiler.
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_basic_fns() {
        assert_eq!(
            test_body(state_for("fn test(a, b) { b = 17 + a; }")),
            vec![
                "push 17",     // Start calculating the rvalue, push the literal
                "peekr", // This is looking up the "a" arg, at frame + 0
                "loadw",
                "add",         // 17 + a
                "peekr", // Calculate the lvalue
                "add 3",       // "b" arg is frame + 3
                "storew",      // Finally store
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_args_preamble() {
        assert_eq!(
            test_preamble(state_for("fn test(a, b) { b = 17 + a; }")),
            vec![
                "dup", // Create pool ptr
                "add 6",
                "pushr", // Store pool ptr
                "pushr", // Store frame ptr
                "peekr", // Capture var b
                "add 3",
                "storew",
                "peekr", // Capture var a
                "storew",
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_args_outro() {
        assert_eq!(
            test_outro(state_for("fn test(a, b) { b = 17 + a; }")),
            vec![
                "push 0", // Default return value if we fall through
                "_forge_gensym_2:", // outro label
                "popr", "pop", // Toss the frame ptr
                "popr", "pop", // Toss the pool ptr
                "ret", // Actually return
            ]
                .join("\n")
        )
    }
}