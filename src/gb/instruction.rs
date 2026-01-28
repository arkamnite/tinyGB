/* Operand Description */

use std::{collections::HashMap, iter::zip, u8};

use strum_macros::{EnumDiscriminants, IntoStaticStr};

use crate::{
    asm::encoding::Encodable,
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

impl Encodable for Operand {
    fn encode(&self) -> u8 {
        match self {
            Operand::ARegister(aregister) => aregister.encode(),
            Operand::CRegisterPtr(cregister_ptr) => cregister_ptr.encode(),
            Operand::Register8(register8) => register8.encode(),
            Operand::RegisterPtr8(register_ptr8) => todo!(),
            Operand::Register16(register16) => register16.encode(),
            Operand::RegisterPtr16(register_ptr16) => todo!(),
            Operand::HLRegisterPtr(hlregister_ptr) => todo!(),
            Operand::PushPopRegister16(push_pop_register16) => push_pop_register16.encode(),
            Operand::Imm8(n) => *n,
            Operand::Imm16(n) => todo!(),
            Operand::Ptr8(p) => todo!(),
            Operand::Ptr16(p) => todo!(),
            Operand::SignedImm4(n) => todo!(),
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
            Token::IntegerPointer(addr) => Ok(Operand::Ptr16(addr)),
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
    pub fn encode(&self, operands: &[Operand]) -> Result<Vec<u8>, (String, Opcode)> {
        let mut bytes: Vec<u8> = vec![];
        dbg!(self.opcode, operands);
        if operands.len() != self.operand_descriptions.len() {
            return Err((
                ("Incorrect number of operands provided for instruction!").to_string(),
                self.opcode,
            ));
        }
        match self.opcode {
            Opcode::Nop => {
                bytes.push(self.base);
                Ok(bytes)
            }
            Opcode::Stop => {
                bytes.push(self.base);
                Ok(bytes)
            }
            Opcode::LoadRdRr => {
                if let (Operand::Register8(rd), Operand::Register8(rr)) =
                    (operands[0].clone(), operands[1].clone())
                {
                    bytes.push(
                        self.base
                            | rd.encode() << self.operand_descriptions[0].shift
                            | rr.encode() << self.operand_descriptions[1].shift,
                    );
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        Opcode::LoadRdRr,
                    ))
                }
            }
            Opcode::LoadRdImm8 => todo!(),
            Opcode::LoadRdHLRegPtr => todo!(),
            Opcode::LoadHLRegPtrRr => todo!(),
            Opcode::LoadHLRegPtrImm8 => todo!(),
            Opcode::LoadARegPtr16 => todo!(),
            Opcode::LoadRegPtr16AReg => todo!(),
            Opcode::LoadRdRegPtr8 => {
                if let (Some(Operand::ARegister(_)), Operand::CRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        Opcode::LoadRdRegPtr8,
                    ))
                }
            }
            Opcode::LoadRegPtr8Rr => {
                if let (
                    Operand::CRegisterPtr(CRegisterPtr::C),
                    Some(Operand::ARegister(ARegister::A)),
                ) = (
                    operands[0].clone(),
                    operands[1].clone().try_register8_to_aregister(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        Opcode::LoadRegPtr8Rr,
                    ))
                }
            }
            Opcode::LoadRdImmPtr8 => todo!(),
            Opcode::LoadImmPtr8Rr => todo!(),
            Opcode::LoadRdImmPtr16 => todo!(),
            Opcode::LoadImmPtr16Rr => todo!(),
            Opcode::LoadIncRdRegPtr16 => todo!(),
            Opcode::LoadDecRdRegPtr16 => todo!(),
            Opcode::LoadIncRegPtr16Rr => todo!(),
            Opcode::LoadDecRegPtr16Rr => todo!(),
            Opcode::JumpPtr16 => {
                if let Operand::Ptr16(addr) = operands[0] {
                    bytes.push(self.base);
                    bytes.push(addr as u8);
                    bytes.push((addr >> 8) as u8);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        Opcode::Stop,
                    ))
                }
            }
            Opcode::AddARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AddARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AddARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AddCarryARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AddCarryARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AddCarryARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::SubARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::SubARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::SubARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::SubCarryARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::SubCarryARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::SubCarryARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AndARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AndARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::AndARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::OrARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::OrARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::OrARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::XorARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::XorARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::XorARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::CpARegRr => {
                if let (Some(Operand::ARegister(_)), Operand::Register8(rr)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base | rr.encode());
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::CpARegImm8 => {
                if let (Some(Operand::ARegister(_)), Operand::Imm8(n)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    bytes.push(n);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::CpARegHLRegPtr => {
                if let (Some(Operand::ARegister(_)), Operand::HLRegisterPtr(_)) = (
                    operands[0].clone().try_register8_to_aregister(),
                    operands[1].clone(),
                ) {
                    bytes.push(self.base);
                    Ok(bytes)
                } else {
                    Err((
                        ("Incorrect operands provided for instruction!").to_string(),
                        self.opcode,
                    ))
                }
            }
            Opcode::IncRd => todo!(),
            Opcode::IncHLRegPtr => todo!(),
            Opcode::DecRd => todo!(),
            Opcode::DecHLRegPtr => todo!(),
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

    // 8-bit arithmetic + logic instructions
    // add %a, %rr
    AddARegRr,
    // add %a, n
    AddARegImm8,
    // add %a, $hl
    AddARegHLRegPtr,

    // adc %a, %rr
    AddCarryARegRr,
    // adc %a, n
    AddCarryARegImm8,
    // adc %a, $hl
    AddCarryARegHLRegPtr,

    // sub %a, %rr
    SubARegRr,
    // sub %a, n
    SubARegImm8,
    // sub %a, $hl
    SubARegHLRegPtr,

    // sbc %a, %rr
    SubCarryARegRr,
    // sbc %a, n
    SubCarryARegImm8,
    // sbc %a, $hl
    SubCarryARegHLRegPtr,

    // and %a, %rr
    AndARegRr,
    // and %a, n
    AndARegImm8,
    // and %a, $hl
    AndARegHLRegPtr,

    // or %a, %rr
    OrARegRr,
    // or %a, n
    OrARegImm8,
    // or %a, $hl
    OrARegHLRegPtr,

    // xor %a, %rr
    XorARegRr,
    // xor %a, n
    XorARegImm8,
    // xor %a, $hl
    XorARegHLRegPtr,

    // cp %a, %rr
    CpARegRr,
    // cp %a, n
    CpARegImm8,
    // cp %a, $hl
    CpARegHLRegPtr,

    // inc %rd
    IncRd,
    // inc $hl
    IncHLRegPtr,
    // dec %rd
    DecRd,
    // dec $hl
    DecHLRegPtr,

    // jp $nn
    JumpPtr16,
}

fn try_operands(descriptions: &[OperandDesc], operands: &[Operand]) -> bool {
    if descriptions.len() == operands.len() {
        zip(descriptions, operands).fold(true, |acc, (desc, op)| {
            acc && desc.operand_type == OperandDiscriminants::from(op)
        })
    } else {
        false
    }
}

static OPCODES: &[InstructionDesc] = &[
    InstructionDesc {
        generic_opcode: GeneralOpcode::Nop,
        opcode: Opcode::Nop,
        base: 0b00000000,
        operand_descriptions: &[],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Stop,
        opcode: Opcode::Stop,
        base: 0b00010000,
        operand_descriptions: &[],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdRr,
        base: 0b01000000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 3,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
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
    InstructionDesc {
        generic_opcode: GeneralOpcode::Add,
        opcode: Opcode::AddARegRr,
        base: 0b10000000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Add,
        opcode: Opcode::AddARegImm8,
        base: 0b11000110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Add,
        opcode: Opcode::AddARegHLRegPtr,
        base: 0b10000110,
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
        generic_opcode: GeneralOpcode::AddCarry,
        opcode: Opcode::AddCarryARegRr,
        base: 0b10001000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::AddCarry,
        opcode: Opcode::AddCarryARegImm8,
        base: 0b11001110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::AddCarry,
        opcode: Opcode::AddCarryARegHLRegPtr,
        base: 0b10001110,
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
        generic_opcode: GeneralOpcode::Sub,
        opcode: Opcode::SubARegRr,
        base: 0b10010000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Sub,
        opcode: Opcode::SubARegImm8,
        base: 0b11010110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Sub,
        opcode: Opcode::SubARegHLRegPtr,
        base: 0b10010110,
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
        generic_opcode: GeneralOpcode::SubCarry,
        opcode: Opcode::SubCarryARegRr,
        base: 0b10011000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::SubCarry,
        opcode: Opcode::SubCarryARegImm8,
        base: 0b11011110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::SubCarry,
        opcode: Opcode::SubCarryARegHLRegPtr,
        base: 0b10011110,
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
        generic_opcode: GeneralOpcode::And,
        opcode: Opcode::AndARegRr,
        base: 0b10100000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::And,
        opcode: Opcode::AndARegImm8,
        base: 0b11100110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::And,
        opcode: Opcode::AndARegHLRegPtr,
        base: 0b10100110,
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
        generic_opcode: GeneralOpcode::Or,
        opcode: Opcode::OrARegRr,
        base: 0b10110000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Or,
        opcode: Opcode::OrARegImm8,
        base: 0b11110110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Or,
        opcode: Opcode::OrARegHLRegPtr,
        base: 0b10110110,
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
        generic_opcode: GeneralOpcode::Xor,
        opcode: Opcode::XorARegRr,
        base: 0b10101000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Xor,
        opcode: Opcode::XorARegImm8,
        base: 0b11101110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Xor,
        opcode: Opcode::XorARegHLRegPtr,
        base: 0b10101110,
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
        generic_opcode: GeneralOpcode::Compare,
        opcode: Opcode::CpARegRr,
        base: 0b10111000,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Register8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Compare,
        opcode: Opcode::CpARegImm8,
        base: 0b11111110,
        operand_descriptions: &[
            OperandDesc {
                operand_type: OperandDiscriminants::ARegister,
                shift: 0,
            },
            OperandDesc {
                operand_type: OperandDiscriminants::Imm8,
                shift: 0,
            },
        ],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Compare,
        opcode: Opcode::CpARegHLRegPtr,
        base: 0b10111110,
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
        generic_opcode: GeneralOpcode::Increment,
        opcode: Opcode::IncRd,
        base: 0b00000100,
        operand_descriptions: &[OperandDesc {
            operand_type: OperandDiscriminants::Register8,
            shift: 3,
        }],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Increment,
        opcode: Opcode::IncHLRegPtr,
        base: 0b00110100,
        operand_descriptions: &[OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 0,
        }],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Decrement,
        opcode: Opcode::DecRd,
        base: 0b00000101,
        operand_descriptions: &[OperandDesc {
            operand_type: OperandDiscriminants::Register8,
            shift: 3,
        }],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Decrement,
        opcode: Opcode::DecHLRegPtr,
        base: 0b00110101,
        operand_descriptions: &[OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 0,
        }],
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Jump,
        opcode: Opcode::JumpPtr16,
        base: 0b11000011,
        operand_descriptions: &[OperandDesc {
            operand_type: OperandDiscriminants::Ptr16,
            shift: 0,
        }],
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_encode_nop() {
        let nop_opcode_desc = OPCODEMAP.get(&Opcode::Nop);
        match nop_opcode_desc {
            Some(desc) => {
                assert_eq!(desc.encode(&[]).ok(), Some(vec![0b00000000]));
                assert_eq!(desc.encode(&[Operand::Imm8(0)]).ok(), None);
            }
            None => {
                assert!(false, "Couldn't find NOP opcode in table!")
            }
        }
    }

    #[test]
    fn test_encode_stop() {
        let stop_opcode_desc = OPCODEMAP.get(&Opcode::Stop);
        match stop_opcode_desc {
            Some(desc) => {
                assert_eq!(desc.encode(&[]).ok(), Some(vec![0b00010000]));
                assert_eq!(desc.encode(&[Operand::Imm8(0)]).ok(), None);
            }
            None => {
                assert!(false, "Couldn't find STOP opcode in table!")
            }
        }
    }

    #[test]
    fn test_encode_jp_a16() {
        let jp_opcode_desc = OPCODEMAP.get(&Opcode::JumpPtr16);
        match jp_opcode_desc {
            Some(desc) => {
                assert_eq!(desc.encode(&[]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Imm8(0)]).ok(), None);
                assert_eq!(
                    desc.encode(&[Operand::Ptr16(0x0150)]).ok(),
                    Some(vec![0b11000011, 0x50, 0x1])
                );
            }
            None => {
                assert!(false, "Couldn't find JumpPtr16 opcode in table!")
            }
        }
    }

    #[test]
    fn test_encode_ldrdrr() {
        let opcode_desc = OPCODEMAP.get(&Opcode::LoadRdRr);
        match opcode_desc {
            Some(desc) => {
                assert_eq!(desc.encode(&[]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Imm8(0)]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Ptr16(0x0150)]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Register8(Register8::A)]).ok(), None);
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::Register16(Register16::Bc)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::Register8(Register8::B)
                    ])
                    .ok(),
                    Some(vec![0b01111000])
                );
            }
            None => {
                assert!(false, "Couldn't find LoadRdRr opcode in table!")
            }
        }
    }

    #[test]
    fn test_encode_ldrdregptr8() {
        let opcode_desc = OPCODEMAP.get(&Opcode::LoadRdRegPtr8);
        match opcode_desc {
            Some(desc) => {
                assert_eq!(desc.encode(&[]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Imm8(0)]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Ptr16(0x0150)]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Register8(Register8::A)]).ok(), None);
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::Register16(Register16::Bc)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::Register8(Register8::B)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::C),
                        Operand::RegisterPtr8(RegisterPtr8::A)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::RegisterPtr8(RegisterPtr8::C)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::CRegisterPtr(CRegisterPtr::C)
                    ])
                    .ok(),
                    Some(vec![0b11110010])
                );
            }
            None => {
                assert!(false, "Couldn't find LoadRdRegPtr8 opcode in table!")
            }
        }
    }

    #[test]
    fn test_encode_ldregptr8rr() {
        let opcode_desc = OPCODEMAP.get(&Opcode::LoadRegPtr8Rr);
        match opcode_desc {
            Some(desc) => {
                assert_eq!(desc.encode(&[]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Imm8(0)]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Ptr16(0x0150)]).ok(), None);
                assert_eq!(desc.encode(&[Operand::Register8(Register8::A)]).ok(), None);
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::Register16(Register16::Bc)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::A),
                        Operand::Register8(Register8::B)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::Register8(Register8::C),
                        Operand::RegisterPtr8(RegisterPtr8::A)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::RegisterPtr8(RegisterPtr8::C),
                        Operand::Register8(Register8::A)
                    ])
                    .ok(),
                    None
                );
                assert_eq!(
                    desc.encode(&[
                        Operand::CRegisterPtr(CRegisterPtr::C),
                        Operand::Register8(Register8::A)
                    ])
                    .ok(),
                    Some(vec![0b11100010])
                );
            }
            None => {
                assert!(false, "Couldn't find LoadRegPtr8Rr opcode in table!")
            }
        }
    }
}
