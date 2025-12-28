/* Operand Description */

use std::{collections::HashMap, u8};

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

impl TryFrom<Token> for Operand {
    type Error = (&'static str, Token);

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::StringLiteral => todo!(),
            Token::Identifier => todo!(),
            Token::Register(register) => match register {
                crate::gb::register::Register::A => Ok(Operand::ARegister(ARegister::A)),
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

#[derive(Clone)]
pub struct OperandDesc {
    pub operand_type: OperandDiscriminants,
    shift: u8,
}

#[derive(Clone)]
/// A more forward-compatible definition would
/// be to use an array of OperandDesc, and then
/// specify indices for these in the table to help
/// identify input and output operands. This will
/// allow for ISAs which have multiple (input)
/// operands for instructions.
pub struct InstructionDesc {
    opcode: Opcode,
    generic_opcode: GeneralOpcode,
    base: u8,
    input_desc: Option<OperandDesc>,
    output_desc: Option<OperandDesc>,
}

impl InstructionDesc {
    pub fn encode(&self, operands: &[Operand]) -> Result<Vec<u8>, ParserError> {
        let mut bytes: Vec<u8> = vec![];
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

// how are we going to group the instructions
// for the parser? a.k.a. the parser will just
// see the identifier string. It won't know at
// that point what operands there are etc., and
// thus won't know the difference between diff.
// load opcodes. Do we need to define variants
// on the identifiers themselves, i.e. a very
// limited set:
//
// - ld
// - add, sub
// - nop, stop
// - etc.?
//
// These can probably live in the parser end; we
// just need to define a higher level of variant
// that groups all of these, and provide a match
// function.
//
// fetch map: Key = Opcode, Value = InstructionDesc
//
// match fn: GeneralOpcode, InOpDisc, OutOpDisc -> Option<Opcode>
// - strum-generated 'from' func on operands provides OpDisc
// - lives in the parser; needs to iterate through values of the
//   map to find a key that matches these two operand descriptions.
// - the parser GeneralOpcode has to be used to distinguish between
//   the same operand classes being used across add, load etc.
// - an acceptable level of ambiguity is between different general
//   opcodes but not within the same general opcode. Otherwise, what's
//   the point in having different opcodes to begin with?
//
// encoding fn: InstructionDesc, InOperand, OutOperand -> Option<Vec<u8>>
// - verify legal operands
// - output operands as separate bytes if needed (i.e. imm values)
//
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
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Register8,
            shift: 2,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Register8,
            shift: 5,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdImm8,
        base: 0b00000110,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Register8,
            shift: 2,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Imm8,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdHLRegPtr,
        base: 0b01000110,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Register8,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 5,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadHLRegPtrRr,
        base: 0b01110000,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 2,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Register8,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadHLRegPtrImm8,
        base: 0b01110000,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Imm8,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadARegPtr16,
        base: 0b00001010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::RegisterPtr16,
            shift: 5,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRegPtr16AReg,
        base: 0b00000010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::RegisterPtr16,
            shift: 5,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdRegPtr8,
        base: 0b11110010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::CRegisterPtr,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRegPtr8Rr,
        base: 0b11100010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::CRegisterPtr,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdImmPtr8,
        base: 0b11110000,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Ptr8,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadImmPtr8Rr,
        base: 0b11100000,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Ptr8,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadRdImmPtr16,
        base: 0b11111010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Ptr16,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadImmPtr16Rr,
        base: 0b11101010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::Ptr16,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadIncRdRegPtr16,
        base: 0b00101010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadDecRdRegPtr16,
        base: 0b00111010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadIncRegPtr16Rr,
        base: 0b00100010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 0,
        }),
    },
    InstructionDesc {
        generic_opcode: GeneralOpcode::Load,
        opcode: Opcode::LoadDecRegPtr16Rr,
        base: 0b00110010,
        input_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::ARegister,
            shift: 0,
        }),
        output_desc: Some(OperandDesc {
            operand_type: OperandDiscriminants::HLRegisterPtr,
            shift: 0,
        }),
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

fn match_description(
    instruction: Option<InstructionDesc>,
    input_operand: Option<OperandDiscriminants>,
    output_operand: Option<OperandDiscriminants>,
) -> bool {
    let match_input = match (instruction.clone(), input_operand) {
        (None, None) => false,
        (Some(ins_desc), Some(op_desc)) => {
            if let Some(equals) = ins_desc
                .input_desc
                .and_then(|f| Some(f.operand_type == op_desc))
            {
                equals
            } else {
                false
            }
        }
        (_, _) => false,
    };

    let match_output = match (instruction, input_operand) {
        (None, None) => false,
        (Some(ins_desc), Some(op_desc)) => {
            if let Some(equals) = ins_desc
                .input_desc
                .and_then(|f| Some(f.operand_type == op_desc))
            {
                equals
            } else {
                false
            }
        }
        (_, _) => false,
    };

    match_input && match_output
}

pub fn find_instruction(
    general_op: GeneralOpcode,
    input: Option<OperandDiscriminants>,
    output: Option<OperandDiscriminants>,
) -> Option<&'static InstructionDesc> {
    // Get all the keys which match the operand discr. and the general opcode.
    let result = OPCODEMAP.iter().find(|(_, &value)| {
        match_description(Some(value.clone()), input, output) && value.generic_opcode == general_op
    });

    match result {
        Some((_, desc)) => Some(&desc),
        None => None,
    }
}
