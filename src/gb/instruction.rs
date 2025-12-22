/* Operand Description */

use crate::gb::register::{
    PushPopRegister16, Register16, Register8, RegisterAddr16, RegisterAddr8,
};

#[derive(Debug, Clone, Copy)]
enum Opcode {
    Nop,
    Stop,
    Load,
}

// #[derive(Debug)]
/// Different types that an operand can take.
/// TODO: Macro to generate another wrapped enum
/// that can be used to construct 'Instruction',
/// i.e. a struct that has actual values for these variants.
///
/// Do we need this if we are going to use InstructionEncoding?
// pub enum OperandType {
//     Register8,
//     RegisterAddr8,
//     Register16,
//     RegisterAddr16,
//     PushPopRegister16,
// }

pub enum Operand {
    Register8(Register8),
    RegisterAddr8(RegisterAddr8),
    Register16(Register16),
    RegisterAddr16(RegisterAddr16),
    PushPopRegister16(PushPopRegister16),
}

pub enum InstructionEncoding {
    Fixed(u8),
    RegReg {
        base: u8,
        dst_shift: u8,
        src_shift: u8,
    },
    RegImm8 {
        base: u8,
        dst_shift: u8,
    },
    Imm8 {
        base: u8,
    },
    Reg16Imm16 {
        base: u8,
        dst_shift: u8,
    },
    SignedImm8 {
        base: u8,
    },
    Imm16 {
        base: u8,
    },
    Reg {
        base: u8,
        reg_shift: u8,
    },
    Reg16 {
        base: u8,
        reg_shift: u8,
    },
}

/// Encode each of the above formats an instruction can take
/// Emit byte(s), plural if there are immediate operands for
/// example.
/// Will probably need to look at the above and ensure there
/// are no accidental duplicat
impl InstructionEncoding {
    fn encode(&self, operand_types: &[Operand]) -> Vec<u8> {
        match self {}
    }
}

/* Instructions */
pub struct OpcodeDesc {
    mnemonic: &'static str,
    opcode: Opcode,
    operands: &'static [OperandType],
}

#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode,
    operands: Vec<OperandType>,
    encoding: u8,
}

static OPCODES: &[OpcodeDesc] = &[
    OpcodeDesc {
        mnemonic: "nop",
        opcode: Opcode::Nop,
        operands: &[],
    },
    OpcodeDesc {
        mnemonic: "stop",
        opcode: Opcode::Stop,
        operands: &[],
    },
    // Use the GBPM to work out the encoding.
    OpcodeDesc {
        mnemonic: "ld",
        opcode: Opcode::LdRdRr,
        operands: &[OperandType::Register8, OperandType::Register8],
    },
];
