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
mod text;

///////////////////////////////////////////////////////////

/// Turn a `Program` into a list of assembly lines that can be assembled to run it.
/// There are various ways to build a program, this one is the simplest: it builds it as a
/// complete ROM that will place a jmp to main() at 0x400, so a Vulcan can boot from it.
pub fn build_boot(src: &str, include_comments: bool) -> Result<Vec<String>, CompileError> {
    let mut state = if include_comments {
        State::with_comments()
    } else {
        State::default()
    };
    let ast = parse(src).map_err(CompileError::from)?;
    ast.process(&mut state, None, (0, 0).into())?;

    if let Some(Variable::DirectLabel(label)) = state.global_scope.get("main") {
        // Let's make a vec for the final listing
        let mut asm: Vec<String> = Vec::new();

        // Now we start piling stuff into the vec, starting with an org:
        asm.push(".org 0x400".into());

        // We need to put in any global initialization:
        asm.append(state.init.0.as_mut());

        // Main takes no args, but it does take a frame ptr:
        asm.push("push stack".into());

        // jmp into main:
        asm.push(format!("call {}", label));

        // When main returns, just hlt:
        asm.push("hlt".into());

        // Now start dumping compiled objects into there. First functions:
        for (name, val) in state.functions.iter_mut() {
            // If we've asked for comments, then put in a header comment
            if include_comments {
                asm.push(format!(";;; Begin {:=<70}", format!("{} ", name)))
            }
            asm.push(format!("{}:", val.label));
            for line in val.text() {
                asm.push(String::from(line));
            }
            // If we've asked for comments, then put in a footer also
            if include_comments {
                asm.push(format!(";;; End {:=<72}", format!("{} ", name)))
            }
        }

        // Strings:
        for (label, val) in state.strings.iter() {
            let cleaned = val.replace("\\", "\\\\")
                .replace("\t","\\t")
                .replace("\r","\\r")
                .replace("\n","\\n")
                .replace("\0","\\0")
                .replace("\"","\\\"");
            asm.push(format!("{}: .db \"{}\\0\"", label, cleaned))
        }

        // Global vars:
        for (name, val) in state.global_scope.iter() {
            // All global variables are indirect labels; anything in scope that's not that is a const,
            // fn, or string, which we'll emit elsewhere.
            if let Variable::IndirectLabel(label) = val {
                if include_comments {
                    asm.push(format!("{}: .db 0 ;;; Global {}", label, name))
                } else {
                    asm.push(format!("{}: .db 0", label))
                }
            }
        }

        // Static buffers:
        for(label, size) in state.buffers {
            asm.push(format!("{}: .db 0", label));
            asm.push(format!(".org {} + {}", label, size));
        }

        // Once flags:
        for label in state.flags {
            asm.push(format!("{}: .db 1", label));
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
        let asm = build_boot("fn main() { return 5; }".into(), false).unwrap();
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
        let asm = build_boot("const str = \"blah\"; fn main() { return str; }".into(), false).unwrap();
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
    fn test_comments() {
        // Very basic test of one main()
        let asm = build_boot("global foo; fn main() { return 5; }".into(), true).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "push stack",
            "call _forge_gensym_2",
            "hlt",
            ";;; Begin main =================================================================",
            "_forge_gensym_2:",
            "dup",
            "pushr",
            "pushr",
            ";; 1:25 :: return",
            "push 5",
            "jmpr @_forge_gensym_3",
            "push 0",
            "_forge_gensym_3:",
            "popr",
            "pop",
            "popr",
            "pop",
            "ret",
            ";;; End main ===================================================================",
            "_forge_gensym_1: .db 0 ;;; Global foo",
            "stack: .db 0",
        ].join("\n"));
    }
        #[test]
    fn test_global_vars() {
        // Trying a non-str global var
        let asm = build_boot("global foo; fn main() { foo = 3; }".into(), false).unwrap();
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
        let asm = build_boot("fn foo() { } fn main() { foo(); }".into(), false).unwrap();
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

    #[test]
    fn test_global_static() {
        let asm = build_boot("global a = static(10); fn main() { a[2] = 5; }".into(), false).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "push _forge_gensym_2", // the addr of the static buffer
            "storew _forge_gensym_1", // stored in a
            "push stack",
            "call _forge_gensym_3",
            "hlt",
            "_forge_gensym_3:", // fn main()
            "dup", "pushr", "pushr", // capture pool / frame ptrs
            "push 5", // rvalue
            "loadw _forge_gensym_1", "push 2", "mul 3", "add", "storew", // store it in a[2]
            "push 0", // Implicit return value
            "_forge_gensym_4:", // Outro start
            "popr", "pop", "popr", "pop", // Drop frame / pool ptrs
            "ret",
            "_forge_gensym_1: .db 0", // a itself
            "_forge_gensym_2: .db 0", // the buffer
            ".org _forge_gensym_2 + 30",
            "stack: .db 0",
        ].join("\n"))
    }

    #[test]
    fn test_string_escapes() {
        let asm = build_boot("global a = \"\\\\foo\"; fn main() { }".into(), false).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "push _forge_gensym_2",
            "storew _forge_gensym_1",
            "push stack",
            "call _forge_gensym_3",
            "hlt",
            "_forge_gensym_3:", // fn main()
            "dup", "pushr", "pushr", // capture pool / frame ptrs
            "push 0", // Implicit return value
            "_forge_gensym_4:", // Outro start
            "popr", "pop", "popr", "pop", // Drop frame / pool ptrs
            "ret",
            "_forge_gensym_2: .db \"\\\\foo\\0\"", // here
            "_forge_gensym_1: .db 0",
            "stack: .db 0",
        ].join("\n"))
    }
}
