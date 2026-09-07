use novaforth::ROM;
use vcore::{Word, CPU};
use vcore::memory::{PeekPoke, PeekPokeExt};
use crate::constants::{SCREEN, SYMBOLS, TIB};
use crate::memory_item::{MemoryItem, PointerTarget};

pub fn init_cpu() -> CPU {
    let mut cpu = CPU::new_random();
    for (i, b) in ROM.iter().enumerate() {
        cpu.poke(Word::from(0x400 + i), *b)
    }
    cpu
}

#[allow(unused)]
pub trait TestHarness {
    fn run_prelude(&mut self) -> &mut Self;
    fn test_fn(&mut self, name: &str) -> &mut Self;
    fn test_line(&mut self, line: &str) -> &mut Self;

    fn given_stack<W: Into<Word>, I: IntoIterator<Item=W>>(&mut self, stack: I) -> &mut Self;
    fn given_memory<W: Into<Word>>(&mut self, addr: W, value: &str) -> &mut Self;

    fn heap_bytes(&self, base: &str, offset: u32, len: u32) -> Vec<u8>;

    fn expect_stack<W: Into<Word>, I: IntoIterator<Item=W>>(&self, stack: I) -> &Self;
    fn expect_empty_stack(&self) -> &Self;
    fn expect_rstack<W: Into<Word>, I: IntoIterator<Item=W>>(&self, stack: I) -> &Self;
    fn expect_empty_rstack(&self) -> &Self;
    fn expect_output(&self, output: &str) -> &Self;
    fn expect_memory<H: IntoIterator<Item=MemoryItem>>(&self, at: &str, items: H) -> u32;
    fn expect_heap<H: IntoIterator<Item=MemoryItem>>(&self, items: H) -> &Self;
    fn expect_pad<H: IntoIterator<Item=MemoryItem>>(&self, items: H) -> &Self;
    fn expect_4th_rstack<H: IntoIterator<Item=MemoryItem>>(&self, items: H) -> &Self;
    fn expect_pointer<T: Into<PointerTarget>>(&self, symbol: &str, target: T) -> &Self;
    fn expect_var(&self, symbol: &str, value: u32) -> &Self;
    fn expect_cursor(&self, offset: i32) -> &Self;
}

impl TestHarness for CPU {
    fn run_prelude(&mut self) -> &mut Self {
        self.test_line(novaforth::PRELUDE)
    }

    fn test_fn(&mut self, name: &str) -> &mut Self {
        self.push_call(SYMBOLS["stop"]);
        self.set_pc(SYMBOLS[name]);
        self.run_to_halt();
        self
    }

    fn test_line(&mut self, line: &str) -> &mut Self {
        self.given_memory(TIB, line).given_stack([TIB]).test_fn("eval")
    }

    fn given_stack<W: Into<Word>, I: IntoIterator<Item=W>>(&mut self, stack: I) -> &mut Self {
        for val in stack {
            self.push_data(val.into());
        }
        self
    }

    fn given_memory<W: Into<Word>>(&mut self, addr: W, val: &str) -> &mut Self {
        let addr = addr.into();
        for (i, c) in val.chars().enumerate() {
            self.poke8(addr + i as u32, c as u8);
        }
        self.poke8(addr + val.len() as u32, 0u8);
        self
    }

    fn heap_bytes(&self, base: &str, offset: u32, len: u32) -> Vec<u8> {
        let heap: u32 = SYMBOLS[base].into();
        let mut bytes = Vec::with_capacity(len as usize);
        for n in 0..len {
            bytes.push(self.peek8(n + heap + offset))
        }
        bytes
    }

    fn expect_stack<W: Into<Word>, I: IntoIterator<Item=W>>(&self, stack: I) -> &Self {
        let actual = self.get_stack();
        let expected = stack.into_iter().map(|w| w.into()).collect::<Vec<Word>>();
        assert_eq!(actual, expected);
        self
    }

    fn expect_empty_stack(&self) -> &Self {
        assert!(self.get_stack().is_empty());
        self
    }

    fn expect_rstack<W: Into<Word>, I: IntoIterator<Item=W>>(&self, stack: I) -> &Self {
        let actual = self.get_call();
        let expected = stack.into_iter().map(|w| w.into()).collect::<Vec<Word>>();
        assert_eq!(actual, expected);
        self
    }

    fn expect_empty_rstack(&self) -> &Self {
        assert!(self.get_call().is_empty());
        self
    }

    fn expect_output(&self, expected: &str) -> &Self {
        let len: u32 = self.peek24(SYMBOLS["emit_cursor"]).into();
        let mut actual = String::with_capacity(len as usize);
        for a in 0..len {
            actual.push(self.peek8(SCREEN + a) as char);
        }
        assert_eq!(expected, actual);
        self
    }

    fn expect_memory<H: IntoIterator<Item=MemoryItem>>(&self, at: &str, items: H) -> u32 {
        let mut delta = 0u32;

        for item in items {
            if let Err(()) = item.check(self, at, Word::from(delta)) {
                let s = self.heap_bytes(at, delta, item.len()).into_iter().map(|b| format!("0x{:02X}", b)).collect::<Vec<_>>().join(", ");
                let exp_str = item.bytes(self).into_iter().map(|b| format!("0x{:02X}", b)).collect::<Vec<_>>().join(", ");
                panic!("Memory mismatch at {} + {}:\n\texpected {}\n\t\t{}\n\tactual\n\t\t{}", at, delta, item, exp_str, s)
            }
            delta += item.len();
        }

        delta
    }

    fn expect_heap<H: IntoIterator<Item=MemoryItem>>(&self, items: H) -> &Self {
        let delta = self.expect_memory("heap_start", items);
        assert_eq!(SYMBOLS["heap_start"] + delta, self.peek24(SYMBOLS["heap"]));
        self
    }

    fn expect_pad<H: IntoIterator<Item=MemoryItem>>(&self, items: H) -> &Self {
        self.expect_memory("pad", items);
        self
    }

    fn expect_4th_rstack<H: IntoIterator<Item=MemoryItem>>(&self, items: H) -> &Self {
        let delta = self.expect_memory("r_stack", items);
        assert_eq!(SYMBOLS["r_stack"] + delta, self.peek24(SYMBOLS["r_stack_ptr"]));
        self
    }

    fn expect_pointer<T: Into<PointerTarget>>(&self, symbol: &str, target: T) -> &Self {
        let actual = self.peek24(SYMBOLS[symbol]);
        let expected = target.into().addr(self);
        assert_eq!(expected, actual);
        self
    }

    fn expect_var(&self, symbol: &str, value: u32) -> &Self {
        let actual: u32 = self.peek24(SYMBOLS[symbol]).into();
        assert_eq!(value, actual);
        self
    }

    fn expect_cursor(&self, offset: i32) -> &Self {
        let expected = (TIB as i32 + offset) as u32;
        let actual: u32 = self.peek24(SYMBOLS["cursor"]).into();
        assert_eq!(expected, actual);
        self
    }
}