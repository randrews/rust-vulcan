use crate::ast::{Assignment, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Assignment {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let Assignment { lvalue, rvalue } = self;
        let sig = sig.expect("Assignment outside function");
        // First the value, then the address we'll storew it to
        rvalue.process(state, Some(sig), loc)?;
        lvalue.process(state, Some(sig), loc)?;
        sig.emit("storew");
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::test_utils::*;

    #[test]
    fn test_subscripts() {
        assert_eq!(
            test_body(state_for("fn test(x) { x[3] = 5; }")),
            vec![
                "push 5", // rvalue
                "peekr", "loadw", // Base of x
                "push 3", // index
                "mul 3", // as byte offset
                "add", // addr of x[3]
                "storew"
            ].join("\n")
        )
    }

    #[test]
    fn test_zero_subscripts() {
        assert_eq!(
            test_body(state_for("fn test() { var x = new(5); x[0] = 5; }")),
            vec![
                "push 5", // size of array
                "mul 3","popr","swap","popr","swap","pick 1","add","pushr","swap","pushr", // alloc call
                "peekr","storew", // store addr in x
                "push 5", // rvalue
                "peekr", "loadw", // Base of x
                "push 0", // index
                "mul 3", // as byte offset
                "add", // addr of x[0]
                "storew"
            ].join("\n")
        )
    }
}