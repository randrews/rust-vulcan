use vasm_core::assemble_snippet;
use vcore::memory::Memory;
use vcore::CPU;

fn run_forge(src: &str) -> CPU {
    let asm = forge_core::compiler::build_boot(src).unwrap();
    let bin = assemble_snippet(asm).unwrap();
    let mut cpu = CPU::new(Memory::with_program(bin));
    cpu.run_to_halt();
    cpu
}

fn compiler_output(src: &str) -> Vec<String> {
    forge_core::compiler::build_boot(src).unwrap()
}

fn main_return(src: &str) -> i32 {
    let mut cpu = run_forge(src);
    cpu.pop_data().into()
}

fn error_message(src: &str) -> Option<String> {
    match forge_core::compiler::build_boot(src) {
        Ok(_) => None,
        Err(forge_core::compiler::CompileError(_, _, s)) => Some(s)
    }
}

#[test]
fn basic_forge_test() {
    assert_eq!(
        main_return("fn main() { return 2 + 3; }"),
        5
    )
}

#[test]
fn loop_test() {
    assert_eq!(
        main_return(
            "fn triangle(rows) {
                    var total = 0;
                    repeat (rows) n {
                        total = total + (n + 1);
                    }
                    return total;
                 }

                 fn main() { return triangle(5); }"),
        15
    )
}

#[test]
fn arg_test() {
    // Send in two args where the order matters, to ensure that args are being captured correctly
    assert_eq!(
        main_return(
            "fn sub(a, b) { return a - b; }
                 fn main() { return sub(5, 3); }"),
        2
    )
}

#[test]
fn scope_test() {
    // Refer to a var that's now out of scope, to ensure block scoping works
    assert_eq!(
        error_message(
            "fn main() {
               if (1) { var a = 3; } // scoped to the block!
               return a; // should error!
             }"),
        Some("Unknown name a".into())
    );

    // Refer to a var that's still in scope, to ensure we're not wiping too much out
    assert_eq!(
        error_message(
            "fn main(a) {
               if (1) { var b = 3; } // scoped to the block!
               return a; // should work, we didn't remove the thing that dropped
             }"),
        None
    )
}

#[test]
fn recursion_test() {
    // Recursive call with some stuff in it
    assert_eq!(
        main_return(
            "fn sum(n) {
                    if (n > 0) {
                        return n + sum(n - 1);
                    } else {
                        return 0;
                    }
                }
                 fn main() { return sum(5); }"),
        15
    )
}

//#[test]
// This is no longer cursed, or a test. The revised calling convention with the pool pointer makes
// it now perfectly sane. Left here for posterity.
fn _cursed_call_test() {
    // This is a cursed subtlety of block scoping and the way calls are compiled. It's not actually
    // a bug. It's explained below:
    let src = "
        fn foo() { return 2; }
        fn main() {
            repeat (2) n {
                if (n == 1) { foo(); }
                var x = 0;
                if (n == 0) { x = 1; }
            }
        }
    ";

    assert_eq!(main_return(src), 0); // It runs

    assert_eq!(compiler_output(src).join("\n"), vec![
        ".org 0x400",
        "push stack",
        "call _forge_gensym_3",
        "hlt",
        "_forge_gensym_1:", // foo()
        "dup", "pushr", "pushr","push 2","jmpr @_forge_gensym_2","push 0","_forge_gensym_2:","popr","pop", "popr","pop","ret",
        "_forge_gensym_3:", // main()
        "dup", "add 6", "pushr", "pushr", // preamble (no args, but 3 locals)
        "push 0", "peekr", "storew", // repeat counter var (n)
        "push 2", // repeat limit
        "#while",
        "dup", "peekr", "loadw", "sub", "agt 0", // check repeat limit (2 - n > 0)
        "#do",
        "peekr","loadw","push 1","xor","not","#if", // if n == 1
        "peekr","add 3","push _forge_gensym_1","call","pop", // call foo, notice the 3-byte stack
        // frame, which leaves room for n but not anything else
        "#end",
        "push 0","peekr","add 3","storew", // Declare x, store 0 in it (n and x are now in scope)
        "peekr","loadw","push 0","xor","not","#if", // If n == 0
        // This is the trap: x is allocated at frame+3, but the call above is passing frame+3 as foo's
        // frame pointer. It doesn't yet know that x will exist so it can't reserve space for it. But!
        // This is not actually a problem because during that call, x is not actually in scope; in order
        // for this to occur, x has to both enter _and leave_ scope between now and when the call is
        // executed. If it enters scope but doesn't leave, then compiling the call will take it into
        // account. Because it leaves, if it gets clobbered it can't matter. There's no way for flow
        // of control to pass back to here, after adding a local, without that local leaving scope:
        // the loop structures are all block-level, there's no goto.
        "push 1","peekr","add 3","storew", // store 1 in x.
        "#end",
        "peekr","dup","loadw","add 1","swap","storew","#end", // inc n, end the loop
        "pop", // Drop the loop limit off
        "push 0", // Implicit return value
        "_forge_gensym_4:",
        "popr","pop","popr","pop","ret", // Standard outro
        "stack: .db 0", // The stack
    ].join("\n"))
}