use vasm_core::assemble_snippet;
use vcore::memory::{Memory, PeekPokeExt};
use vcore::word::Word;
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

#[test]
fn basic_forge_test() {
    assert_eq!(
        main_return("fn main() { return 2 + 3; }"),
        5
    )
}

// TODO The reason this fails is that functions don't copy their args to the frame when they enter.
// The fix is for fns to know their arity and to do that copy before the body, probably as some
// subroutine call itself (push frame, push arity, call blah, which is written in asm)
// #[test]
// fn loop_test() {
//     assert_eq!(
//         main_return(
//             "fn triangle(rows) {
//                     var total = 0;
//                     repeat (rows) n {
//                         total = total + (n + 1);
//                     }
//                     return total;
//                  }
//
//                  fn main() { return triangle(5); }"),
//         15
//     )
// }