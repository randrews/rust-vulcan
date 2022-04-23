use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Opcode {
    Nop,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Rand,
    And,
    Or,
    Xor,
    Not,
    Gt,
    Lt,
    Agt,
    Alt,
    Lshift,
    Rshift,
    Arshift,
    Pop,
    Dup,
    Swap,
    Pick,
    Rot,
    Jmp,
    Jmpr,
    Call,
    Ret,
    Brz,
    Brnz,
    Hlt,
    Load,
    Loadw,
    Store,
    Storew,
    Setint,
    Setiv,
    Sdp,
    Setsdp,
    Pushr,
    Popr,
    Peekr,
    Debug,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InvalidOpcode(pub u8);

impl Display for InvalidOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid opcode {:#02x}", self.0)
    }
}

impl std::error::Error for InvalidOpcode {}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InvalidMnemonic<'a>(pub &'a str);

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Nop => {
                write!(f, "nop")
            }
            Opcode::Add => {
                write!(f, "add")
            }
            Opcode::Sub => {
                write!(f, "sub")
            }
            Opcode::Mul => {
                write!(f, "mul")
            }
            Opcode::Div => {
                write!(f, "div")
            }
            Opcode::Mod => {
                write!(f, "mod")
            }
            Opcode::Rand => {
                write!(f, "rand")
            }
            Opcode::And => {
                write!(f, "and")
            }
            Opcode::Or => {
                write!(f, "or")
            }
            Opcode::Xor => {
                write!(f, "xor")
            }
            Opcode::Not => {
                write!(f, "not")
            }
            Opcode::Gt => {
                write!(f, "gt")
            }
            Opcode::Lt => {
                write!(f, "lt")
            }
            Opcode::Agt => {
                write!(f, "agt")
            }
            Opcode::Alt => {
                write!(f, "alt")
            }
            Opcode::Lshift => {
                write!(f, "lshift")
            }
            Opcode::Rshift => {
                write!(f, "rshift")
            }
            Opcode::Arshift => {
                write!(f, "arshift")
            }
            Opcode::Pop => {
                write!(f, "pop")
            }
            Opcode::Dup => {
                write!(f, "dup")
            }
            Opcode::Swap => {
                write!(f, "swap")
            }
            Opcode::Pick => {
                write!(f, "pick")
            }
            Opcode::Rot => {
                write!(f, "rot")
            }
            Opcode::Jmp => {
                write!(f, "jmp")
            }
            Opcode::Jmpr => {
                write!(f, "jmpr")
            }
            Opcode::Call => {
                write!(f, "call")
            }
            Opcode::Ret => {
                write!(f, "ret")
            }
            Opcode::Brz => {
                write!(f, "brz")
            }
            Opcode::Brnz => {
                write!(f, "brnz")
            }
            Opcode::Hlt => {
                write!(f, "hlt")
            }
            Opcode::Load => {
                write!(f, "load")
            }
            Opcode::Loadw => {
                write!(f, "loadw")
            }
            Opcode::Store => {
                write!(f, "store")
            }
            Opcode::Storew => {
                write!(f, "storew")
            }
            Opcode::Setint => {
                write!(f, "setint")
            }
            Opcode::Setiv => {
                write!(f, "setiv")
            }
            Opcode::Sdp => {
                write!(f, "sdp")
            }
            Opcode::Setsdp => {
                write!(f, "setsdp")
            }
            Opcode::Pushr => {
                write!(f, "pushr")
            }
            Opcode::Popr => {
                write!(f, "popr")
            }
            Opcode::Peekr => {
                write!(f, "peekr")
            }
            Opcode::Debug => {
                write!(f, "debug")
            }
        }
    }
}

impl TryFrom<u8> for Opcode {
    type Error = InvalidOpcode;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use Opcode::*;
        Ok(match value {
            0 => Nop,
            1 => Add,
            2 => Sub,
            3 => Mul,
            4 => Div,
            5 => Mod,
            6 => Rand,
            7 => And,
            8 => Or,
            9 => Xor,
            10 => Not,
            11 => Gt,
            12 => Lt,
            13 => Agt,
            14 => Alt,
            15 => Lshift,
            16 => Rshift,
            17 => Arshift,
            18 => Pop,
            19 => Dup,
            20 => Swap,
            21 => Pick,
            22 => Rot,
            23 => Jmp,
            24 => Jmpr,
            25 => Call,
            26 => Ret,
            27 => Brz,
            28 => Brnz,
            29 => Hlt,
            30 => Load,
            31 => Loadw,
            32 => Store,
            33 => Storew,
            34 => Setint,
            35 => Setiv,
            36 => Sdp,
            37 => Setsdp,
            38 => Pushr,
            39 => Popr,
            40 => Peekr,
            41 => Debug,
            other => return Err(InvalidOpcode(other)),
        })
    }
}

impl<'a> TryFrom<&'a str> for Opcode {
    type Error = InvalidMnemonic<'a>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        use Opcode::*;
        Ok(match value {
            "nop" | "push" => Nop,
            "add" => Add,
            "sub" => Sub,
            "mul" => Mul,
            "div" => Div,
            "mod" => Mod,
            "rand" => Rand,
            "and" => And,
            "or" => Or,
            "xor" => Xor,
            "not" => Not,
            "gt" => Gt,
            "lt" => Lt,
            "agt" => Agt,
            "alt" => Alt,
            "lshift" => Lshift,
            "rshift" => Rshift,
            "arshift" => Arshift,
            "pop" => Pop,
            "dup" => Dup,
            "swap" => Swap,
            "pick" => Pick,
            "rot" => Rot,
            "jmp" => Jmp,
            "jmpr" => Jmpr,
            "call" => Call,
            "ret" => Ret,
            "brz" => Brz,
            "brnz" => Brnz,
            "hlt" => Hlt,
            "load" => Load,
            "loadw" => Loadw,
            "store" => Store,
            "storew" => Storew,
            "setint" => Setint,
            "setiv" => Setiv,
            "sdp" => Sdp,
            "setsdp" => Setsdp,
            "pushr" => Pushr,
            "popr" => Popr,
            "peekr" => Peekr,
            "debug" => Debug,
            _ => return Err(InvalidMnemonic(value)),
        })
    }
}

impl From<Opcode> for u8 {
    fn from(opcode: Opcode) -> Self {
        match opcode {
            Opcode::Nop => 0,
            Opcode::Add => 1,
            Opcode::Sub => 2,
            Opcode::Mul => 3,
            Opcode::Div => 4,
            Opcode::Mod => 5,
            Opcode::Rand => 6,
            Opcode::And => 7,
            Opcode::Or => 8,
            Opcode::Xor => 9,
            Opcode::Not => 10,
            Opcode::Gt => 11,
            Opcode::Lt => 12,
            Opcode::Agt => 13,
            Opcode::Alt => 14,
            Opcode::Lshift => 15,
            Opcode::Rshift => 16,
            Opcode::Arshift => 17,
            Opcode::Pop => 18,
            Opcode::Dup => 19,
            Opcode::Swap => 20,
            Opcode::Pick => 21,
            Opcode::Rot => 22,
            Opcode::Jmp => 23,
            Opcode::Jmpr => 24,
            Opcode::Call => 25,
            Opcode::Ret => 26,
            Opcode::Brz => 27,
            Opcode::Brnz => 28,
            Opcode::Hlt => 29,
            Opcode::Load => 30,
            Opcode::Loadw => 31,
            Opcode::Store => 32,
            Opcode::Storew => 33,
            Opcode::Setint => 34,
            Opcode::Setiv => 35,
            Opcode::Sdp => 36,
            Opcode::Setsdp => 37,
            Opcode::Pushr => 38,
            Opcode::Popr => 39,
            Opcode::Peekr => 40,
            Opcode::Debug => 41,
        }
    }
}

#[test]
fn test_decode() {
    assert_eq!(Opcode::try_from(18), Ok(Opcode::Pop));
    //assert_eq!(str::fmt("{}", Opcode::try_from(136).unwrap_err()), Err(InvalidOpcode(136)));
}
