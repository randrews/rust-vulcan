use crate::ast::{Location, Lvalue, VarDecl};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for VarDecl {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Var declaration outside function");
        if self.size.is_some() {
            todo!("Arrays are not yet supported")
        }
        if let Some(initial) = self.initial {
            // If it's got an initial value, we have to compile that before we add
            // the name to scope, or else UB will ensue if it refers to itself:
            initial.process(state, Some(sig), loc)?;
            // But then add it to scope and assign:
            sig.add_local(&self.name)?;
            // We'll just whip up an lvalue real quick...
            Lvalue::from(self.name).process(state, Some(sig), loc)?;
            sig.emit("storew"); // And store the initial value there
        } else {
            // Otherwise, just add it to scope and leave garbage in there:
            sig.add_local(&self.name)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_var_decls() {
        assert_eq!(
            test_body(state_for("fn test() { var a; var b = 7; a = b * 2; }")),
            vec![
                "push 7",      // Start calculating the rvalue, push the literal
                "peekr", // "b" is the second local var at frame + 3
                "add 3",
                "storew",      // Do the initialization
                "peekr", // Now we're evaluating b * 2
                "add 3",       // b is at frame + 3...
                "loadw",       // load it to the stack
                "push 2",
                "mul",         // b * 2 evaluated
                "peekr", // Loading "a" as an lvalue
                "storew",      // doing the assignment
                "popr", "pop", "ret 0" // Implicit void return
            ]
                .join("\n")
        )
    }
}