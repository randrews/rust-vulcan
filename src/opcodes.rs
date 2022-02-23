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
    Inton,
    Intoff,
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
            34 => Inton,
            35 => Intoff,
            36 => Setiv,
            37 => Sdp,
            38 => Setsdp,
            39 => Pushr,
            40 => Popr,
            41 => Peekr,
            42 => Debug,
            other => return Err(InvalidOpcode(other)),
        })
    }
}

#[test]
fn test_decode() {
    assert_eq!(Opcode::try_from(18), Ok(Opcode::Pop));
    //assert_eq!(str::fmt("{}", Opcode::try_from(136).unwrap_err()), Err(InvalidOpcode(136)));
}
