use crate::compiler::compilable::Compilable;
pub use crate::compiler::compile_error::CompileError;
use crate::compiler::state::State;
use crate::compiler::utils::Variable;
use crate::parser::parse;

mod ast_nodes;
mod compile_error;
mod compiled_fn;
mod utils;
mod state;
mod compilable;

#[cfg(test)]
mod test_utils;

///////////////////////////////////////////////////////////

/// Turn a `Program` into a list of assembly lines that can be assembled to run it.
/// There are various ways to build a program, this one is the simplest: it builds it as a
/// complete ROM that will place a jmp to main() at 0x400, so a Vulcan can boot from it.
pub fn build_boot(src: &str) -> Result<Vec<String>, CompileError> {
    let mut state = State::default();
    let ast = parse(src).map_err(CompileError::from)?;
    ast.process(&mut state, None, (0, 0).into())?;

    if let Some(Variable::DirectLabel(label)) = state.global_scope.get("main") {
        // Let's make a vec for the final listing
        let mut asm: Vec<String> = Vec::new();

        // Now we start piling stuff into the vec, starting with an org:
        asm.push(".org 0x400".into());

        // Main takes no args, but it does take a frame ptr:
        asm.push("push stack".into());

        // jmp into main:
        asm.push(format!("call {}", label));

        // When main returns, just hlt:
        asm.push("hlt".into());

        // Now start dumping compiled objects into there. First functions:
        for (_, val) in state.functions.iter_mut() {
            asm.push(format!("{}:", val.label));
            asm.append(val.preamble.as_mut());
            asm.append(val.body.as_mut());
            asm.append(val.outro.as_mut());
        }

        // Strings:
        for (label, val) in state.strings.iter() {
            asm.push(format!("{}: .db \"{}\\0\"", label, val))
        }

        // Global vars:
        for (_, val) in state.global_scope.iter() {
            // All global variables are indirect labels; anything in scope that's not that is a const,
            // fn, or string, which we'll emit elsewhere.
            if let Variable::IndirectLabel(label) = val {
                asm.push(format!("{}: .db 0", label))
            }
        }

        // Static buffers:
        for(label, size) in state.buffers {
            asm.push(format!("{}: .db 0", label));
            asm.push(format!(".org {} + {}", label, size));
        }

        // Final thing is to place a label for the stack:
        // (this is just a cell with the address of the following word)
        asm.push("stack: .db 0".into());
        Ok(asm)
    } else {
        Err(CompileError(0, 0, "Function main not defined (or not a function)".into()))
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_build_boot() {
        // Very basic test of one main()
        let asm = build_boot("fn main() { return 5; }".into()).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "push stack",
            "call _forge_gensym_1",
            "hlt",
            "_forge_gensym_1:",
            "dup",
            "pushr",
            "pushr",
            "push 5",
            "jmpr @_forge_gensym_2",
            "push 0",
            "_forge_gensym_2:",
            "popr",
            "pop",
            "popr",
            "pop",
            "ret",
            "stack: .db 0",
        ].join("\n"));

        // Slightly more complicated, with a global str
        let asm = build_boot("const str = \"blah\"; fn main() { return str; }".into()).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "push stack",
            "call _forge_gensym_2",
            "hlt",
            "_forge_gensym_2:",
            "dup",
            "pushr",
            "pushr",
            "push _forge_gensym_1", // Push the str
            "jmpr @_forge_gensym_3", // Return
            "push 0", // Implicit return val
            "_forge_gensym_3:", // outro
            "popr",
            "pop",
            "popr",
            "pop",
            "ret",
            "_forge_gensym_1: .db \"blah\\0\"",
            "stack: .db 0",
        ].join("\n"))
    }

    #[test]
    fn test_global_vars() {
        // Trying a non-str global var
        let asm = build_boot("global foo; fn main() { foo = 3; }".into()).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "push stack",
            "call _forge_gensym_2",
            "hlt",
            "_forge_gensym_2:", // main()
            "dup", "pushr", "pushr", // The frame is zero length; that's a global we're assigning to
            "push 3", // rvalue
            "push _forge_gensym_1", // lvalue
            "storew",
            "push 0",
            "_forge_gensym_3:",
            "popr", "pop", "popr", "pop", "ret",
            "_forge_gensym_1: .db 0", // The global var
            "stack: .db 0",
        ].join("\n"))
    }

    #[test]
    fn test_no_return() {
        let asm = build_boot("fn foo() { } fn main() { foo(); }".into()).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "push stack",
            "call _forge_gensym_3",
            "hlt",
            "_forge_gensym_1:", // fn foo()
            "dup", "pushr", // capture pool ptr
            "pushr", // capture frame ptr
            "push 0", // implicit return
            "_forge_gensym_2:",
            "popr", "pop",
            "popr", "pop",
            "ret",
            "_forge_gensym_3:", // fn main()
            "dup", "pushr", // capture pool ptr
            "pushr", // capture frame ptr
            "popr", "peekr", "swap", "pushr", // Grab pool ptr to send to foo
            "push _forge_gensym_1", // load foo
            "call", // call it
            "pop", // Throw away its return value
            "push 0", // Implicit return value
            "_forge_gensym_4:", // Outro start
            "popr", // Drop frame ptr
            "pop",
            "popr", // Drop pool ptr
            "pop",
            "ret",
            "stack: .db 0",
        ].join("\n"))
    }
}
