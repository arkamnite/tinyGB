use crate::gb::asm::{Encodable, PrintAsm};

type TryFromRegisterError = (&'static str, Register);

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
pub enum RegisterA {
    A,
}

impl Encodable for RegisterA {
    fn encode(&self) -> u8 {
        match self {
            RegisterA::A => todo!(),
        }
    }
}

impl TryFrom<Register> for RegisterA {
    type Error = TryFromRegisterError;
    fn try_from(value: Register) -> Result<Self, Self::Error> {
        match value {
            Register::A => Ok(RegisterA::A),
            _ => Err(("Register other than 'A' provided!", value)),
        }
    }
}

// Might want to replace the Encodable trait impl
// for strum property (int)?
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
    fn encode(&self) -> u8 {
        match self {
            Register8::B => 0b000,
            Register8::C => 0b001,
            Register8::D => 0b010,
            Register8::E => 0b011,
            Register8::H => 0b100,
            Register8::L => 0b101,
            Register8::A => 0b111,
        }
    }
}

impl TryFrom<Register> for Register8 {
    type Error = TryFromRegisterError;
    fn try_from(value: Register) -> Result<Self, Self::Error> {
        match value {
            Register::A => Ok(Register8::A),
            Register::B => Ok(Register8::B),
            Register::C => Ok(Register8::C),
            Register::D => Ok(Register8::D),
            Register::E => Ok(Register8::E),
            Register::H => Ok(Register8::H),
            Register::L => Ok(Register8::L),
            _ => Err(("Invalid 8-bit register provided!", value)),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum RegisterPtr8 {
    A,
    C,
}

impl TryFrom<Register> for RegisterPtr8 {
    type Error = TryFromRegisterError;
    fn try_from(value: Register) -> Result<Self, Self::Error> {
        match value {
            Register::A => Ok(RegisterPtr8::A),
            Register::C => Ok(RegisterPtr8::C),
            _ => Err(("Invalid 8-bit register pointer provided!", value)),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum Register16 {
    Bc,
    De,
    Hl,
    Sp,
}

impl Encodable for Register16 {
    fn encode(&self) -> u8 {
        match self {
            Register16::Bc => 0b00,
            Register16::De => 0b01,
            Register16::Hl => 0b10,
            Register16::Sp => 0b11,
        }
    }
}

impl PrintAsm for Register16 {
    fn print_asm(&self) -> String {
        (match self {
            Register16::Bc => "bc",
            Register16::De => "de",
            Register16::Hl => "hl",
            Register16::Sp => "sp",
        })
        .to_string()
    }
}

impl TryFrom<Register> for Register16 {
    type Error = TryFromRegisterError;
    fn try_from(value: Register) -> Result<Self, Self::Error> {
        match value {
            Register::BC => Ok(Register16::Bc),
            Register::DE => Ok(Register16::De),
            Register::HL => Ok(Register16::Hl),
            _ => Err(("Invalid 16-bit register provided!", value)),
        }
    }
}

/// TODO: Notably absent here is the HLI and HLD
/// operands; these must be supported later.
#[derive(Debug, PartialEq, Clone, Eq, Copy)]
pub enum RegisterPtr16 {
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
    fn encode(&self) -> u8 {
        match self {
            PushPopRegister16::Bc => 0b00,
            PushPopRegister16::De => 0b01,
            PushPopRegister16::Hl => 0b10,
            PushPopRegister16::Af => 0b11,
        }
    }
}

impl PrintAsm for PushPopRegister16 {
    fn print_asm(&self) -> String {
        (match self {
            PushPopRegister16::Bc => "bc",
            PushPopRegister16::De => "de",
            PushPopRegister16::Hl => "hl",
            PushPopRegister16::Af => "af",
        })
        .to_string()
    }
}
