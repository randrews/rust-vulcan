use std::fmt::Display;
use vcore::opcodes::Opcode;
use vcore::{Word, CPU};
use vcore::memory::PeekPokeExt;
use crate::constants::SYMBOLS;

pub enum MemoryItem {
    /// A string, null-terminated
    String(String),
    /// A pointer to somewhere
    Pointer(PointerTarget),
    /// A literal word
    Value(Word),
    /// An instruction, maybe containing an argument
    Instruction(Opcode, Option<Box<MemoryItem>>),
    /// An opcode, not including the arg length flags that an instruction has
    Opcode(Opcode),
    /// Skip some stuff we don't want to both asserting
    Skip(u32)
}

impl Display for MemoryItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryItem::String(s) => write!(f, "str({})", s),
            MemoryItem::Pointer(p) => write!(f, "ptr({})", p),
            MemoryItem::Value(v) => write!(f, "num({})", v),
            MemoryItem::Instruction(opcode, Some(arg)) => write!(f, "inst({}, {})", opcode, arg),
            MemoryItem::Instruction(opcode, None) => write!(f, "inst({})", opcode),
            MemoryItem::Opcode(opcode) => write!(f, "{}", opcode),
            MemoryItem::Skip(len) => write!(f, "skip({})", len),
        }
    }
}

impl MemoryItem {
    /// Asserts that this heap item is found at the given offset from the heap ptr in the given CPU
    pub fn check<W: Into<Word>>(&self, cpu: &CPU, base_sym: &str, offset: W) -> Result<(), ()> {
        let base: u32 = SYMBOLS[base_sym].into();
        let offset: u32 = offset.into().into();
        match self {
            &MemoryItem::String(ref expected) => {
                let mut actual = String::with_capacity(expected.len());
                let mut curr = base + offset;
                while cpu.peek8(curr) != 0 {
                    actual.push(cpu.peek8(curr) as char);
                    curr += 1
                }

                if expected != &actual { Err(()) } else { Ok(()) }
            }

            &MemoryItem::Pointer(ref expected) => {
                let expected = expected.addr(cpu);
                let actual: u32 = cpu.peek24(base + offset).into();
                if expected != actual { Err(()) } else { Ok(()) }
            }

            &MemoryItem::Instruction(ref opcode, ref arg) => {
                let actual_op = cpu.peek8(base + offset);
                if *opcode != Opcode::try_from(actual_op / 4).unwrap() { return Err(()) }
                if let Some(arg) = arg {
                    if actual_op & 0x3 != 3 as u8 { Err(()) } else {
                        arg.check(cpu, base_sym, offset + 1)
                    }
                } else {
                    if actual_op & 0x3 != 0 { Err(()) } else { Ok(()) }
                }
            }

            &MemoryItem::Opcode(ref opcode) => {
                let actual_val = cpu.peek8(base + offset);
                if u8::from(*opcode) == actual_val { Ok(()) } else { Err(()) }
            }

            &MemoryItem::Value(ref val) => {
                let actual = cpu.peek24(base + offset);
                if *val != actual { Err(()) } else { Ok(()) }
            }

            &MemoryItem::Skip(_) => { Ok(()) }
        }
    }

    pub fn len(&self) -> u32 {
        match self {
            MemoryItem::String(s) => s.len() as u32 + 1, // Add the null terminator
            MemoryItem::Pointer(_) | MemoryItem::Value(_) => 3, // Any pointer is 3 long
            MemoryItem::Instruction(_, Some(_)) => 4, // Any instruction with an arg
            MemoryItem::Instruction(_, None) => 1, // No arg
            MemoryItem::Opcode(_) => 1,
            MemoryItem::Skip(size) => *size,
        }
    }

    pub fn bytes(&self, cpu: &CPU) -> Vec<u8> {
        match self {
            &MemoryItem::String(ref s) => s.as_bytes().to_vec(),
            &MemoryItem::Pointer(ref p) => Vec::from(p.addr(cpu).to_bytes()),
            MemoryItem::Instruction(op, Some(arg)) => { // Any instruction with an arg
                let mut v = vec![u8::from(*op) * 4 + arg.len() as u8];
                v.extend(arg.bytes(cpu));
                v
            },
            MemoryItem::Opcode(op) => vec![u8::from(*op)],
            MemoryItem::Instruction(op, None) => vec![u8::from(*op) * 4], // No arg
            &MemoryItem::Value(ref v) => Vec::from(v.to_bytes()),
            &MemoryItem::Skip(_) => vec![],
        }
    }
}

pub enum PointerTarget {
    /// An absolute address
    Absolute(Word),
    /// The address of a symbol
    Symbol(String),
    /// An offset from the start of the heap
    Heap(Word),
    /// Whatever the new heap pointer is
    NewHeap,
}

impl Display for PointerTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PointerTarget::Absolute(a) => write!(f, "<{}>", a),
            PointerTarget::Symbol(s) => write!(f, "<{}: {}>", s, SYMBOLS[s]),
            PointerTarget::Heap(h) => write!(f, "<heap + {}: {}>", h, SYMBOLS["heap_start"] + *h),
            PointerTarget::NewHeap => write!(f, "<newheap>"),
        }
    }
}

impl From<Word> for PointerTarget {
    fn from(word: Word) -> Self { Self::Absolute(word) }
}

impl From<&str> for PointerTarget {
    fn from(word: &str) -> Self { Self::Symbol(word.to_string()) }
}

pub fn ascii(s: &str) -> MemoryItem { MemoryItem::String(s.to_owned()) }
pub fn ptr<P: Into<PointerTarget>>(val: P) -> MemoryItem { MemoryItem::Pointer(val.into()) }
pub fn heap<W: Into<Word>>(val: W) -> PointerTarget { PointerTarget::Heap(val.into()) }
pub fn new_heap() -> PointerTarget { PointerTarget::NewHeap}
pub fn op(mnemonic: &str) -> MemoryItem { MemoryItem::Opcode(Opcode::try_from(mnemonic).unwrap()) }
pub fn num<W: Into<Word>>(val: W) -> MemoryItem { MemoryItem::Value(val.into()) }
pub fn inst4<H: Into<MemoryItem>>(mnemonic: &str, arg: H) -> MemoryItem { MemoryItem::Instruction(Opcode::try_from(mnemonic).unwrap(), Some(Box::new(arg.into()))) }
pub fn inst1(mnemonic: &str) -> MemoryItem { MemoryItem::Instruction(Opcode::try_from(mnemonic).unwrap(), None) }
pub fn skip(size: u32) -> MemoryItem { MemoryItem::Skip(size) }

impl Into<MemoryItem> for i32 {
    fn into(self) -> MemoryItem { MemoryItem::Value(self.into()) }
}

impl PointerTarget {
    pub fn addr(&self, cpu: &CPU) -> Word {
        match self {
            &Self::Absolute(addr) => addr,
            &Self::Symbol(ref name) => SYMBOLS[name],
            &Self::Heap(offset) => SYMBOLS["heap_start"] + offset,
            &Self::NewHeap => cpu.peek24(SYMBOLS["heap"]),
        }
    }
}