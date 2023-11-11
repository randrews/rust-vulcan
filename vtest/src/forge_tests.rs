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