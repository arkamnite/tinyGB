/* Operand Description */

use std::{collections::HashMap, iter::zip, u8};

use strum_macros::{EnumDiscriminants, IntoStaticStr};

use crate::{
    gb::register::{
        ARegister, CRegisterPtr, HLRegisterPtr, PushPopRegister16, Register16, Register8,
        RegisterPtr16, RegisterPtr8,
    },
    lexer::Token,
    parser::{GeneralOpcode, ParserError},
};

#[derive(Debug, Clone, EnumDiscriminants, Eq, PartialEq)]
pub enum Operand {
    ARegister(ARegister),
    CRegisterPtr(CRegisterPtr),
    Register8(Register8),
    RegisterPtr8(RegisterPtr8),
    Register16(Register16),
    RegisterPtr16(RegisterPtr16),
    HLRegisterPtr(HLRegisterPtr),
    PushPopRegister16(PushPopRegister16),
    Imm8(u8),
    Imm16(u16),
    Ptr8(u8),
    Ptr16(u16),
    SignedImm4(u8),
}

impl Operand {
    fn try_register8_to_aregister(&self) -> Option<Operand> {
        match self {
            Operand::Register8(register8) => match register8 {
                Register8::A => Some(Operand::ARegister(ARegister::A)),
                _ => None,
            },
            _ => None,
        }
    }
}

impl TryFrom<Token> for Operand {
    type Error = (&'static str, Token);

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::StringLiteral => todo!(),
            Token::Identifier => todo!(),
            Token::Register(register) => match register {
                crate::gb::register::Register::A => Ok(Operand::Register8(Register8::A)),
                crate::gb::register::Register::B => Ok(Operand::Register8(Register8::B)),
                crate::gb::register::Register::C => Ok(Operand::Register8(Register8::C)),
                crate::gb::register::Register::D => Ok(Operand::Register8(Register8::D)),
                crate::gb::register::Register::E => Ok(Operand::Register8(Register8::E)),
                crate::gb::register::Register::H => Ok(Operand::Register8(Register8::H)),
                crate::gb::register::Register::L => Ok(Operand::Register8(Register8::L)),
                _ => Err(("Register pair provided!", value)),
            },
            Token::RegisterPtr(register) => match register {
                crate::gb::register::Register::C => Ok(Operand::CRegisterPtr(CRegisterPtr::C)),
                _ => Err(("Invalid 8-bit register used as pointer!", value)),
            },
            Token::Register16(register) => match register {
                crate::gb::register::Register::BC => Ok(Operand::Register16(Register16::Bc)),
                crate::gb::register::Register::DE => Ok(Operand::Register16(Register16::De)),
                crate::gb::register::Register::HL => Ok(Operand::Register16(Register16::Hl)),
                crate::gb::register::Register::PC => todo!(),
                crate::gb::register::Register::SP => todo!(),
                _ => Err(("Invalid 8-bit register used for register pair!", value)),
            },
            Token::RegisterPtr16(register) => match register {
                crate::gb::register::Register::BC => Ok(Operand::RegisterPtr16(RegisterPtr16::Bc)),
                crate::gb::register::Register::DE => Ok(Operand::RegisterPtr16(RegisterPtr16::De)),
                _ => Err(("Only BC and DE register pairs can be used as general-purpose 16-bit register pointers", value))
            },
            Token::Integer(_) => todo!(),
            Token::IntegerPointer(_) => todo!(),
            _ => Err(("Token does not map to operand!", value)),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct OperandDesc {
    pub operand_type: OperandDiscriminants,
    shift: u8,
}

#[derive(Clone)]
pub struct InstructionDesc {
    opcode: Opcode,
    generic_opcode: GeneralOpcode,
    base: u8,
    operand_descriptions: &'static [OperandDesc],
}

impl InstructionDesc {
    pub fn encode(&self, operands: &[Operand]) -> Result<Vec<u8>, ParserError> {
        let mut bytes: Vec<u8> = vec![];
        println!("Opcode: {:?}, Operands: {:?}", self.opcode, operands);
        match self.opcode {
            Opcode::Nop => todo!(),
            Opcode::Stop => todo!(),
            Opcode::LoadRdRr => todo!(),
            Opcode::LoadRdImm8 => todo!(),
            Opcode::LoadRdHLRegPtr => todo!(),
            Opcode::LoadHLRegPtrRr => todo!(),
            Opcode::LoadHLRegPtrImm8 => todo!(),
            Opcode::LoadARegPtr16 => todo!(),
            Opcode::LoadRegPtr16AReg => todo!(),
            Opcode::LoadRdRegPtr8 => todo!(),
            Opcode::LoadRegPtr8Rr => todo!(),
            Opcode::LoadRdImmPtr8 => todo!(),
            Opcode::LoadImmPtr8Rr => todo!(),
            Opcode::LoadRdImmPtr16 => todo!(),
            Opcode::LoadImmPtr16Rr => todo!(),
            Opcode::LoadIncRdRegPtr16 => todo!(),
            Opcode::LoadDecRdRegPtr16 => todo!(),
            Opcode::LoadIncRegPtr16Rr => todo!(),
            Opcode::LoadDecRegPtr16Rr => todo!(),
        }
    }
}

// note for later: might be a good idea to use a builder of sorts
// for the byte(s). This could help simplify the legalisation logic?

#[derive(Debug, Clone, Copy, IntoStaticStr, Eq, Hash, PartialEq)]
pub enum Opcode {
    Nop,
    Stop,

    /* 8-bit Transfer and Input/Output */
    // ld %rd, %rr
    LoadRdRr,

    // ld %rd, n
    LoadRdImm8,

    // ld %rd, $hl
    LoadRdHLRegPtr,
    // ld $hl, %rr
    LoadHLRegPtrRr,

    // ld $hl, n
    LoadHLRegPtrImm8,

    // ld %a, {$bc, $de}
    LoadARegPtr16,
    // ld {$bc, $de}, %a
    LoadRegPtr16AReg,

    // ld %a, $c
    LoadRdRegPtr8,
    // ld $c, %a
    LoadRegPtr8Rr,

    // ld %a, $n
    LoadRdImmPtr8,
    // ld $n, %a
    LoadImmPtr8Rr,

    // ld %a, $nn
    LoadRdImmPtr16,
    // ld $nn, %a
    LoadImmPtr16Rr,

    // ldi %a, $hl
    LoadIncRdRegPtr16,
    // ldd %a, $hl
    LoadDecRdRegPtr16,

    // ldi $hl, %a
    LoadIncRegPtr16Rr,
    // ldd $hl, %a
    LoadDecRegPtr16Rr,
}

static OPCODES: &[InstructionDesc] = &[
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdRr,
        base: 0b01000000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 5,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 2,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdImm8,
        base: 0b00000110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 2,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdHLRegPtr,
        base: 0b01000110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 5,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::HLRegisterPtr,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadHLRegPtrRr,
        base: 0b01110000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::HLRegisterPtr,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 2,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadHLRegPtrImm8,
        base: 0b01110000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::HLRegisterPtr,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Ptr8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadARegPtr16,
        base: 0b00001010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::RegisterPtr16,
                shift: 5,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRegPtr16AReg,
        base: 0b00000010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::RegisterPtr16,
                shift: 5,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdRegPtr8,
        base: 0b11110010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::CRegisterPtr,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRegPtr8Rr,
        base: 0b11100010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::CRegisterPtr,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 5,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdImmPtr8,
        base: 0b11110000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Ptr8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadImmPtr8Rr,
        base: 0b11100000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::Ptr8,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdImmPtr16,
        base: 0b11111010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Ptr16,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadImmPtr16Rr,
        base: 0b11101010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::Ptr16,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::LoadIncrement,
        opcode: Opcode::LoadIncRdRegPtr16,
        base: 0b00101010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::HLRegisterPtr,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::LoadDecrement,
        opcode: Opcode::LoadDecRdRegPtr16,
        base: 0b00111010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::HLRegisterPtr,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::LoadIncrement,
        opcode: Opcode::LoadIncRegPtr16Rr,
        base: 0b00100010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::HLRegisterPtr,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::LoadDecrement,
        opcode: Opcode::LoadDecRegPtr16Rr,
        base: 0b00110010,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::HLRegisterPtr,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
        ],
    },
];

lazy_static::lazy_static! {
static ref OPCODEMAP: HashMap<Opcode, &'static InstructionDesc> = {
  let mut map: HashMap<Opcode, &'static InstructionDesc> = HashMap::new();

  for op in OPCODES {
    map.entry(op.opcode)
    .or_insert_with(|| op);
  }

  map
};
}

fn check_match_operand(desc: &OperandDesc, operand: &Operand) -> bool {
    let actual_type = desc.operand_type;
    if actual_type != OperandDiscriminants::from(operand) {
        match (desc.operand_type, operand) {
            (OperandDiscriminants::ARegister, Operand::Register8(r)) => {
                match operand.try_register8_to_aregister() {
                    Some(_) => true,
                    None => false,
                }
            }
            _ => false,
        }
    } else {
        true
    }
}

fn match_descriptions(operand_descriptions: &[OperandDesc], operands: &[Operand]) -> bool {
    if operand_descriptions.len() == operands.len() {
        let iter = zip(operand_descriptions, operands);
        iter.fold(true, |acc, (desc, operand)| {
            check_match_operand(desc, operand) && acc
        })
    } else {
        false
    }
}

pub fn find_instruction(
    general_op: GeneralOpcode,
    operands: &Vec<Operand>,
) -> Option<&'static InstructionDesc> {
    let find_result = OPCODEMAP.iter().find(|(_, &value)| {
        value.generic_opcode == general_op
            && match_descriptions(value.operand_descriptions, &operands.as_slice())
    });
    match find_result {
        Some((_, desc)) => Some(desc),
        None => None,
    }
}
