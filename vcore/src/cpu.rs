use crate::memory::PeekPoke;
use crate::memory::{Memory, PeekPokeExt};
use crate::opcodes::InvalidOpcode;
use crate::opcodes::Opcode;
use crate::word::Word;
use std::convert::TryFrom;

#[allow(clippy::upper_case_acronyms)]
pub struct CPU {
    memory: Memory,    // Main memory, all of it
    pc: Word,          // program counter, address of the low byte of the instruction
    dp: Word,          // data pointer, address of the low byte of one cell above the data stack
    sp: Word,          // stack pointer, address of the low byte of the return stack
    iv: [Word; 16],    // interrupt vectors
    int_enabled: bool, // interrupt enable bit
    halted: bool,      // Whether the CPU is halted
    sp_top: Word,      // The last value given for the sp, or 0x400
    dp_btm: Word,      // The last value given for the dp, or 0x100
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Instruction {
    opcode: Opcode,
    arg: Option<Word>,
    length: u8,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum ExecutionError {
    DivZero,
    DataUnderflow,
    StackUnderflow,
    Overflow,
    InvalidOpcode,
}

impl PeekPoke for CPU {
    fn peek(&self, addr: Word) -> u8 {
        self.memory.peek(addr)
    }
    fn poke(&mut self, addr: Word, val: u8) {
        self.memory.poke(addr, val)
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self::new(Memory::default())
    }
}

impl CPU {
    pub fn new(memory: Memory) -> Self {
        Self {
            memory,
            pc: 1024.into(),
            dp: 256.into(),
            sp: 1024.into(),
            iv: [1024.into(); 16],
            int_enabled: false,
            halted: true,
            sp_top: 0x400.into(),
            dp_btm: 0x100.into(),
        }
    }

    pub fn reset(&mut self) {
        self.pc = 1024.into();
        self.dp = 256.into();
        self.sp = 1024.into();
        self.iv = [1024.into(); 16];
        self.int_enabled = false;
        self.halted = true;
        self.sp_top = 0x400.into();
        self.dp_btm = 0x100.into();
    }

    fn push_data<A: Into<Word>>(&mut self, word: A) {
        self.memory.poke24(self.dp, word);
        self.dp += 3;
    }

    fn push_call<A: Into<Word>>(&mut self, word: A) {
        self.sp -= 3;
        self.memory.poke24(self.sp, word);
    }

    fn pop_data(&mut self) -> Word {
        self.dp -= 3;
        self.memory.peek24(self.dp)
    }

    fn pop_call(&mut self) -> Word {
        let val = self.memory.peek24(self.sp);
        self.sp += 3;
        val
    }

    fn peek_call(&self) -> Word {
        self.memory.peek24(self.sp)
    }

    fn peek_data(&self) -> Word {
        self.memory.peek24(self.dp - 3)
    }

    fn fetch(&self) -> Result<Instruction, InvalidOpcode> {
        let instruction = self.memory.peek(self.pc);
        match Opcode::try_from(instruction >> 2) {
            Ok(opcode) => {
                let arg_length = instruction & 3;
                if arg_length == 0 {
                    Ok(Instruction {
                        opcode,
                        arg: None,
                        length: 1,
                    })
                } else {
                    let mut arg = 0u32;
                    for n in 0..arg_length {
                        let mut b: u32 = self.memory.peek(self.pc + (n + 1) as i32) as u32;
                        b <<= 8 * n;
                        arg += b;
                    }
                    Ok(Instruction {
                        opcode,
                        arg: Some(Word::from(arg)),
                        length: arg_length + 1,
                    })
                }
            }
            Err(e) => Err(e),
        }
    }

    #[allow(clippy::cmp_owned)]
    // This is needed because clippy can't figure out, on alt / agt, that even though yes you can
    // directly compare two Words, that means something different than comparing the i32s those
    // Words represent. That difference is the entire point of the agt / alt instructions.
    fn execute(&mut self, instruction: Instruction) -> Word {
        if let Some(arg) = instruction.arg {
            self.push_data(arg)
        }

        if instruction.opcode.arity() == 2 {
            let x = self.pop_data();
            let y = self.pop_data();

            match instruction.opcode {
                Opcode::Add => self.push_data(x + y),
                Opcode::Sub => self.push_data(y - x),
                Opcode::Mul => self.push_data(y * x),
                Opcode::Div => self.push_data(y / x),
                Opcode::Mod => self.push_data(y % x),
                Opcode::And => self.push_data(y & x),
                Opcode::Or => self.push_data(y | x),
                Opcode::Xor => self.push_data(y ^ x),
                Opcode::Gt => self.push_data(y > x),
                Opcode::Lt => self.push_data(y < x),
                Opcode::Agt => self.push_data(i32::from(y) > i32::from(x)),
                Opcode::Alt => self.push_data(i32::from(y) < i32::from(x)),
                Opcode::Lshift => self.push_data(y << x),
                Opcode::Rshift => self.push_data(y >> x),
                Opcode::Arshift => {
                    if y & 0x800000 != 0 {
                        let mut shifted = y;
                        for _ in 0..u32::from(x).clamp(0, 24) {
                            shifted = shifted >> 1 | 0x800000;
                        }
                        self.push_data(shifted)
                    } else {
                        self.push_data(y >> x)
                    }
                }
                Opcode::Swap => {
                    self.push_data(x);
                    self.push_data(y)
                }
                Opcode::Store => self.memory.poke8(x, y.to_bytes()[0]),
                Opcode::Storew => self.memory.poke24(x, y),
                Opcode::Setsdp => {
                    self.dp = x;
                    self.sp = y;
                    self.dp_btm = self.dp;
                    self.sp_top = self.sp
                }
                Opcode::Brz => {
                    if y == 0 {
                        return self.pc + i32::from(x);
                    }
                }
                Opcode::Brnz => {
                    if y != 0 {
                        return self.pc + i32::from(x);
                    }
                }
                Opcode::Setiv => {
                    self.iv[usize::from(x % 16)] = y;
                }
                _ => unreachable!(),
            }
            self.pc + instruction.length as i32
        } else {
            match instruction.opcode {
                Opcode::Nop => { /* No action required */ }
                Opcode::Rand => {
                    todo!()
                }
                Opcode::Not => {
                    let x = self.pop_data();
                    self.push_data(x == 0)
                }
                Opcode::Pop => {
                    self.pop_data();
                }
                Opcode::Dup => self.push_data(self.peek_data()),
                Opcode::Pick => {
                    let index = self.pop_data();
                    let addr = self.dp - (i32::from(index) + 1) * 3;
                    if addr >= self.dp_btm {
                        self.push_data(self.memory.peek24(addr));
                    } else {
                        self.push_data(0);
                    }
                }
                Opcode::Rot => {
                    let x = self.pop_data();
                    let y = self.pop_data();
                    let z = self.pop_data();
                    self.push_data(y);
                    self.push_data(x);
                    self.push_data(z)
                }
                Opcode::Jmp => return self.pop_data(),
                Opcode::Jmpr => {
                    let x = i32::from(self.pop_data());
                    return self.pc + x;
                }
                Opcode::Call => {
                    let x = self.pop_data();
                    self.push_call(self.pc + instruction.length as i32);
                    return x;
                }
                Opcode::Ret => return self.pop_call(),
                Opcode::Hlt => self.halted = true,
                Opcode::Load => {
                    let x = self.pop_data();
                    self.push_data(self.memory.peek8(x))
                }
                Opcode::Loadw => {
                    let x = self.pop_data();
                    self.push_data(self.memory.peek24(x))
                }
                Opcode::Setint => {
                    let x = self.pop_data();
                    self.int_enabled = x != 0;
                }
                Opcode::Sdp => {
                    self.push_data(self.sp);
                    self.push_data(self.dp + 3) // The +3 accounts for the word we're about to push
                }
                Opcode::Pushr => {
                    let x = self.pop_data();
                    self.push_call(x)
                }
                Opcode::Popr => {
                    let r = self.pop_call();
                    self.push_data(r)
                }
                Opcode::Peekr => {
                    let r = self.peek_call();
                    self.push_data(r)
                }
                Opcode::Debug => { /* TODO This should print the stack or something */ }
                _ => {} // This can never happen
            }
            self.pc + instruction.length as i32
        }
    }

    /// Checks if an instruction would cause an error if executed, and returns it if so.
    /// There are four possible error conditions, evaluated in order:
    ///
    /// - If an instruction requires more stack space after execution than exists
    /// - If an instruction would pop more from the data stack than is on it
    /// - If an instruction would pop more from the return stack than is on it
    /// - If an instruction would divide (or modulus) by zero
    ///
    /// So, running `div` on a stack containing a single 0 is a `DataUnderflow`, not a `DivZero`.
    ///
    /// Also, this function doesn't consider the requirements for actually handling the error, which
    /// could change a `DivZero` into an `Overflow`: handling any error consumes one more stack cell,
    /// for the new return address when calling the interrupt handler. So, although this may
    /// return `DivZero` the actual error thrown may be `Overflow`.  
    fn error(&self, instruction: Instruction) -> Option<ExecutionError> {
        use ExecutionError::*;
        use Opcode::{Div, Mod};
        let Instruction { opcode, arg, .. } = instruction;
        let data_height: usize = usize::from(self.dp - self.dp_btm) / 3;
        let stack_height: usize = usize::from(self.sp_top - self.sp) / 3;
        let room: i32 = i32::from(self.sp - self.dp) / 3;
        let net = opcode.net_stack() + arg.map_or(0, |_| 1);

        // If we're out of room, any instruction with an arg is out. Also, if the total amount
        // we'll add to the stack (including arg) overflows, then we overflow
        if arg.is_some() && room < 1 || net > room {
            return Some(Overflow);
        }

        // Figure out if there might be an underflow
        let provided_args = data_height + if arg.is_some() { 1 } else { 0 };

        // Arity taller than data stack?
        if opcode.arity() > provided_args {
            return Some(DataUnderflow);
        }

        // Underflowing the rstack?
        if opcode.r_arity() > stack_height {
            return Some(StackUnderflow);
        }

        // The top of the stack during the instruction
        let top = arg.unwrap_or_else(|| self.peek_data());

        // Dividing by zero?
        if (opcode == Div || opcode == Mod) && top == 0u32 {
            return Some(DivZero);
        }

        None
    }

    fn handle_error(&mut self, error: ExecutionError) -> Word {
        // If the error is not an overflow then we still might turn it into one: we require one new
        // stack frame to handle every non-overflow error (for the new return stack cell) so if we
        // don't have that we'll just pretend this is an overflow.
        if self.dp >= self.sp && error != ExecutionError::Overflow {
            return self.handle_error(ExecutionError::Overflow);
        }

        // The procedure for handling an overflow is a bit different, because we need to have some
        // stack available to do it. First, we reset the stack to its original size, and then we
        // put the old sp and dp on the data stack:
        if error == ExecutionError::Overflow {
            let (old_sp, old_dp) = (self.sp, self.dp);
            (self.sp, self.dp) = (self.sp_top, self.dp_btm);
            self.push_data(old_sp);
            self.push_data(old_dp);
        }

        // Now we have room to handle whatever this is, so, handle it:
        let int = match error {
            ExecutionError::DivZero => 0,
            ExecutionError::DataUnderflow => 1,
            ExecutionError::StackUnderflow => 2,
            ExecutionError::Overflow => 3,
            ExecutionError::InvalidOpcode => 4,
        };
        self.int_enabled = false;
        self.push_call(self.pc);
        self.iv[int]
    }

    pub fn tick(&mut self) {
        if self.halted {
            return;
        }

        match self.fetch() {
            Ok(instr) => {
                self.pc = if let Some(err) = self.error(instr) {
                    self.handle_error(err)
                } else {
                    self.execute(instr)
                }
            }

            Err(_) => self.pc = self.handle_error(ExecutionError::InvalidOpcode),
        }
    }

    pub fn start(&mut self) {
        self.halted = false
    }

    pub fn running(&self) -> bool {
        !self.halted
    }

    pub fn run_to_halt(&mut self) {
        self.start();
        while !self.halted {
            self.tick()
        }
    }

    pub fn get_stack(&self) -> Vec<Word> {
        let mut v = Vec::new();
        let mut curr = self.dp_btm;
        while curr < self.dp {
            v.push(self.memory.peek24(curr));
            curr += 3
        }
        v
    }

    pub fn get_call(&self) -> Vec<Word> {
        let mut v = Vec::new();
        let mut curr = self.sp_top;
        while curr > self.sp {
            curr -= 3;
            v.push(self.memory.peek24(curr));
        }
        v
    }

    pub fn sdp(&self) -> (Word, Word) {
        (self.sp, self.dp)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Opcode::*;

    fn predicate_opcode_test<P, Q>(opcode: Opcode, given: P, pred: Q)
    where
        P: FnOnce(&mut CPU),
        Q: FnOnce(&CPU),
    {
        let mut cpu = CPU::new(Memory::default());
        given(&mut cpu);
        let new_pc = cpu.execute(Instruction {
            opcode,
            arg: None,
            length: 1,
        });
        cpu.pc = new_pc;
        pred(&mut cpu)
    }

    fn simple_opcode_test(given: Vec<u32>, opcode: Opcode, expected: Vec<u32>) {
        predicate_opcode_test(
            opcode,
            |cpu| {
                for i in given.into_iter() {
                    cpu.push_data(i)
                }
            },
            |cpu| assert_eq!(cpu.get_stack(), expected),
        )
    }

    fn call_stack_opcode_test(
        given: Vec<u32>,
        given_r: Vec<u32>,
        opcode: Opcode,
        expected: Vec<u32>,
        expected_r: Vec<u32>,
        pc: Word,
    ) {
        predicate_opcode_test(
            opcode,
            |cpu| {
                for i in given.into_iter() {
                    cpu.push_data(i)
                }
                for i in given_r.into_iter() {
                    cpu.push_call(i)
                }
            },
            |cpu| {
                assert_eq!(cpu.get_stack(), expected);
                assert_eq!(cpu.get_call(), expected_r);
                assert_eq!(pc, cpu.pc)
            },
        )
    }

    fn control_flow_opcode_test<A>(given: Vec<u32>, opcode: Opcode, expected_pc: A)
    where
        A: Into<Word>,
    {
        predicate_opcode_test(
            opcode,
            |cpu| {
                for i in given.into_iter() {
                    cpu.push_data(i)
                }
            },
            |cpu| assert_eq!(cpu.pc, expected_pc.into()),
        )
    }

    fn memory_opcode_test(
        given: Vec<u32>,
        given_memory: Vec<u8>,
        opcode: Opcode,
        expected: Vec<u32>,
        expected_memory: Option<Vec<u8>>,
    ) {
        predicate_opcode_test(
            opcode,
            |cpu| {
                for i in given.into_iter() {
                    cpu.push_data(i)
                }
                for (offset, byte) in given_memory.into_iter().enumerate() {
                    cpu.memory.poke(Word::from(2048 + offset as u32), byte)
                }
            },
            |cpu| {
                if let Some(expected_memory) = expected_memory {
                    for (offset, byte) in expected_memory.into_iter().enumerate() {
                        let actual = cpu.memory.peek(Word::from(2048 + offset as u32));
                        assert_eq!(byte, actual, "At address 2048 + {}", offset)
                    }
                    assert_eq!(cpu.get_stack(), expected)
                }
            },
        )
    }

    fn to_word(val: i32) -> u32 {
        if val >= 0 {
            val as u32
        } else {
            ((-val ^ 0xffffff) + 1) as u32
        }
    }

    #[test]
    fn test_arithmetic() {
        simple_opcode_test(vec![5, 3], Add, vec![8]);
        simple_opcode_test(vec![5, 3], Sub, vec![2]);
        simple_opcode_test(vec![5, 3], Mul, vec![15]);
        simple_opcode_test(vec![8, 3], Div, vec![2]);
        simple_opcode_test(vec![10, 3], Mod, vec![1]);
    }

    #[test]
    fn test_stack_manipulation() {
        simple_opcode_test(vec![5], Dup, vec![5, 5]);
        simple_opcode_test(vec![5, 3], Swap, vec![3, 5]);
        simple_opcode_test(vec![10, 20, 30, 2], Pick, vec![10, 20, 30, 10]);
        simple_opcode_test(vec![10, 0], Pick, vec![10, 10]);
        simple_opcode_test(vec![10, 1], Pick, vec![10, 0]);
        simple_opcode_test(vec![1, 4, 9], Rot, vec![4, 9, 1]);
        simple_opcode_test(vec![1, 4, 9], Pop, vec![1, 4]);
    }

    #[test]
    fn test_basic_ops() {
        control_flow_opcode_test(vec![], Nop, 1025);
        simple_opcode_test(vec![2], Nop, vec![2]);
        predicate_opcode_test(Hlt, |_| {}, |cpu| assert!(cpu.halted))
    }

    #[test]
    fn test_branching_jumping() {
        control_flow_opcode_test(vec![1234], Jmp, 1234);
        control_flow_opcode_test(vec![35], Jmpr, 1024 + 35);
        control_flow_opcode_test(vec![to_word(-3)], Jmpr, 1024 - 3);
        control_flow_opcode_test(vec![0, 35], Brnz, 1024 + 1);
        control_flow_opcode_test(vec![17, 35], Brnz, 1024 + 35);
        control_flow_opcode_test(vec![5, 35], Brz, 1024 + 1);
        control_flow_opcode_test(vec![0, 35], Brz, 1024 + 35);
    }

    #[test]
    fn test_memory() {
        memory_opcode_test(vec![2048], vec![123], Load, vec![123], None);
        memory_opcode_test(
            vec![2048],
            vec![0x12, 0x34, 0x56],
            Loadw,
            vec![0x123456],
            None,
        );
        memory_opcode_test(
            vec![100, 2048],
            vec![0x12, 0x34, 0x56],
            Store,
            vec![],
            Some(vec![100, 0x34, 0x56]),
        );
        memory_opcode_test(
            vec![0x112233, 2048],
            vec![0x12, 0x34, 0x56],
            Storew,
            vec![],
            Some(vec![0x33, 0x22, 0x11]),
        );
    }

    #[test]
    fn test_logic() {
        simple_opcode_test(vec![0b111100, 0b001111], And, vec![0b001100]);
        simple_opcode_test(vec![0b100, 0b001], Or, vec![0b101]);
        simple_opcode_test(vec![0b101, 0b011], Xor, vec![0b110]);
        simple_opcode_test(vec![5], Not, vec![0]);
        simple_opcode_test(vec![0], Not, vec![1]);
        simple_opcode_test(vec![5, 3], Gt, vec![1]);
        simple_opcode_test(vec![5, 7], Gt, vec![0]);
        simple_opcode_test(vec![5, 3], Lt, vec![0]);
        simple_opcode_test(vec![5, 7], Lt, vec![1]);
        simple_opcode_test(vec![5, to_word(-3)], Agt, vec![1]);
        simple_opcode_test(vec![to_word(-3), 5], Agt, vec![0]);
        simple_opcode_test(vec![5, 10], Agt, vec![0]);
        simple_opcode_test(vec![5, to_word(-3)], Alt, vec![0]);
        simple_opcode_test(vec![to_word(-3), 5], Alt, vec![1]);
        simple_opcode_test(vec![5, 10], Alt, vec![1]);
        simple_opcode_test(vec![0b1100, 2], Rshift, vec![3]);
        simple_opcode_test(vec![0b1100, 2], Lshift, vec![0b110000]);
        simple_opcode_test(vec![0x800010, 2], Arshift, vec![0xe00004]);
    }

    #[test]
    fn test_cpu_call_stack() {
        call_stack_opcode_test(vec![5000], vec![], Call, vec![], vec![1025], 5000.into());
        call_stack_opcode_test(vec![], vec![5000], Ret, vec![], vec![], 5000.into());
        call_stack_opcode_test(
            vec![],
            vec![],
            Sdp,
            vec![1024, 256 + 6],
            vec![],
            1025.into(),
        );
        predicate_opcode_test(
            Setsdp,
            |cpu| {
                cpu.push_data(1000u32);
                cpu.push_data(2000u32)
            },
            |cpu| {
                assert_eq!(cpu.sp, 1000);
                assert_eq!(cpu.dp, 2000)
            },
        );
        call_stack_opcode_test(vec![123], vec![], Pushr, vec![], vec![123], 1025.into());
        call_stack_opcode_test(vec![], vec![123], Popr, vec![123], vec![], 1025.into());
        call_stack_opcode_test(vec![], vec![123], Peekr, vec![123], vec![123], 1025.into());
    }

    #[test]
    fn test_cpu_new() {
        let cpu = CPU::new(Memory::default());
        assert_eq!(cpu.pc, 1024);
        assert!(cpu.halted);
    }

    #[test]
    fn test_cpu_reset() {
        let mut cpu = CPU::new(Memory::default());
        cpu.iv[2] = 12345.into();
        cpu.reset();
        assert_eq!(cpu.iv[2], 1024);
    }

    #[test]
    fn test_cpu_stacks() {
        let mut cpu = CPU::new(Memory::default());
        cpu.push_data(37u32);
        cpu.push_data(45u32);
        assert_eq!(cpu.memory.peek24(256), 37);
        assert_eq!(cpu.memory.peek24(259), 45);

        cpu.push_call(12u32);
        cpu.push_call(34u32);
        assert_eq!(cpu.memory.peek24(cpu.sp), 34);
        assert_eq!(cpu.memory.peek24(cpu.sp + 3), 12);
        assert_eq!(cpu.sp, 1024 - 6);
        assert_eq!(cpu.dp, 256 + 6);

        assert_eq!(cpu.pop_data(), 45);
        assert_eq!(cpu.pop_data(), 37);
        assert_eq!(cpu.dp, 256);

        assert_eq!(cpu.pop_call(), 34);
        assert_eq!(cpu.pop_call(), 12);
        assert_eq!(cpu.sp, 1024);
    }

    #[test]
    fn test_cpu_fetch() {
        let mut cpu = CPU::new(Memory::default());
        cpu.memory.poke8(0x400, 0x01); // nop 1 arg
        cpu.memory.poke8(0x401, 0x02); // 2
        cpu.memory.poke8(0x402, 0x07); // add 3 arg
        cpu.memory.poke24(0x403, 0x123456); // 3-byte arg
        cpu.memory.poke8(0x406, 29 << 2); // hlt
        cpu.memory.poke8(0x407, 0xfc); // gibberish

        assert_eq!(
            cpu.fetch(),
            Ok(Instruction {
                opcode: Opcode::Nop,
                arg: Some(Word::from(2)),
                length: 2
            })
        );

        cpu.pc = 0x402.into();
        assert_eq!(
            cpu.fetch(),
            Ok(Instruction {
                opcode: Opcode::Add,
                arg: Some(Word::from(0x123456)),
                length: 4
            })
        );

        cpu.pc = 0x406.into();
        assert_eq!(
            cpu.fetch(),
            Ok(Instruction {
                opcode: Opcode::Hlt,
                arg: None,
                length: 1
            })
        );

        cpu.pc = 0x407.into();
        assert_eq!(cpu.fetch(), Err(InvalidOpcode(0x3f)));
    }
}
