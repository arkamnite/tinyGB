use crate::gb::asm::Encodable;

#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    BC,
    DE,
    HL,
    PC,
    SP,
}

#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum Register8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl Encodable for Register8 {
    fn encode(&self) -> String {
        (match self {
            Register8::B => "000",
            Register8::C => "001",
            Register8::D => "010",
            Register8::E => "011",
            Register8::H => "100",
            Register8::L => "101",
            Register8::A => "111",
        })
        .to_string()
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum RegisterAddr8 {
    A,
    C,
}

#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum Register16 {
    Bc,
    De,
    Hl,
    Sp,
}

impl Encodable for Register16 {
    fn encode(&self) -> String {
        (match self {
            Register16::Bc => "00",
            Register16::De => "01",
            Register16::Hl => "10",
            Register16::Sp => "11",
        })
        .to_string()
    }
}

/// TODO: Notably absent here is the HLI and HLD
/// operands; these must be supported later.
#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum RegisterAddr16 {
    Bc,
    De,
    Hl,
}

#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum PushPopRegister16 {
    Bc,
    De,
    Hl,
    Af,
}

impl Encodable for PushPopRegister16 {
    fn encode(&self) -> String {
        (match self {
            PushPopRegister16::Bc => "00",
            PushPopRegister16::De => "01",
            PushPopRegister16::Hl => "10",
            PushPopRegister16::Af => "11",
        })
        .to_string()
    }
}
