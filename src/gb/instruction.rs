/* Operand Description */

use std::{collections::HashMap, fmt, u8};

use crate::gb::{
    asm::Encodable,
    register::{PushPopRegister16, Register16, Register8, RegisterPtr16, RegisterPtr8},
};

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Nop,
    Stop,
    Load,
    LoadIncrement,
    LoadDecrement,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Nop => write!(f, "nop"),
            Opcode::Stop => write!(f, "stop"),
            Opcode::Load => write!(f, "ld"),
            Opcode::LoadIncrement => write!(f, "ldi"),
            Opcode::LoadDecrement => write!(f, "ldd"),
        }
    }
}

pub fn match_opcode(token: String) -> Option<Opcode> {
    match token.as_str() {
        "nop" => Some(Opcode::Nop),
        "stop" => Some(Opcode::Stop),
        "ld" => Some(Opcode::Load),
        "ldi" => Some(Opcode::LoadIncrement),
        "ldd" => Some(Opcode::LoadDecrement),
        _ => None,
    }
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

#[derive(Debug, Clone)]
pub enum Operand {
    Register8(Register8),
    RegisterPtr8(RegisterPtr8),
    Register16(Register16),
    RegisterPtr16(RegisterPtr16),
    PushPopRegister16(PushPopRegister16),
    Imm8(u8),
    Imm16(u16),
    Ptr8(u8),
    Ptr16(u16),
    SignedImm4(u8),
}

impl Operand {
    pub fn gen_placeholder(&self) -> String {
        (match self {
            Operand::Register8(_) => "r",
            Operand::RegisterPtr8(_) => "$r",
            Operand::Register16(_) => "dd",
            Operand::RegisterPtr16(_) => "$dd",
            Operand::PushPopRegister16(_) => "qq",
            Operand::Imm8(_) => "n",
            Operand::Imm16(_) => "nn",
            Operand::SignedImm4(_) => "e",
            Operand::Ptr8(_) => "$n",
            Operand::Ptr16(_) => "$nn",
        })
        .to_string()
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Register8(_) => write!(f, "r"),
            Operand::RegisterPtr8(_) => write!(f, "$r"),
            Operand::Register16(_) => write!(f, "dd"),
            Operand::RegisterPtr16(_) => write!(f, "$dd"),
            Operand::PushPopRegister16(_) => write!(f, "qq"),
            Operand::Imm8(_) => write!(f, "n"),
            Operand::Imm16(_) => write!(f, "nn"),
            Operand::SignedImm4(_) => write!(f, "e"),
            Operand::Ptr8(_) => write!(f, "$n"),
            Operand::Ptr16(_) => write!(f, "$nn"),
        }
    }
}

pub enum InstructionEncoding {
    Fixed {
        base: u8,
    },
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
    fn encode(&self, operands: &[Operand]) -> Option<Vec<u8>> {
        match self {
            InstructionEncoding::Fixed { base } => Some(vec![*base]),
            InstructionEncoding::RegReg {
                base,
                dst_shift,
                src_shift,
            } => {
                if let (Operand::Register8(r1), Operand::Register8(r2)) =
                    (operands[0].clone(), operands[1].clone())
                {
                    Some(vec![
                        base | r1.encode() << dst_shift | r2.encode() << src_shift,
                    ])
                } else {
                    None
                }
            }
            InstructionEncoding::RegImm8 { base, dst_shift } => {
                if let (Operand::Register8(r1), Operand::Imm8(n)) =
                    (operands[0].clone(), operands[1].clone())
                {
                    Some(vec![base | r1.encode() << dst_shift, n])
                } else {
                    None
                }
            }
            InstructionEncoding::Imm8 { base } => {
                if let Operand::Imm8(n) = operands[0] {
                    Some(vec![*base, n])
                } else {
                    None
                }
            }
            InstructionEncoding::Reg16Imm16 { base, dst_shift } => {
                if let (Operand::Register16(r1), Operand::Imm16(n)) =
                    (operands[0].clone(), operands[1].clone())
                {
                    Some(vec![
                        base | (r1.encode() << dst_shift),
                        n as u8,
                        (n >> 8) as u8,
                    ])
                } else {
                    None
                }
            }
            InstructionEncoding::SignedImm8 { base } => todo!(),
            InstructionEncoding::Imm16 { base } => {
                if let Operand::Imm16(n) = operands[0] {
                    Some(vec![*base, n as u8, (n >> 8) as u8])
                } else {
                    None
                }
            }
            InstructionEncoding::Reg { base, reg_shift } => {
                if let Operand::Register8(r) = operands[0] {
                    Some(vec![base | r.encode() << reg_shift])
                } else {
                    None
                }
            }
            InstructionEncoding::Reg16 { base, reg_shift } => {
                if let Operand::Register16(r) = operands[0] {
                    Some(vec![base | r.encode() << reg_shift])
                } else {
                    None
                }
            }
        }
    }
}

/* Instructions */
pub struct OpcodeDesc {
    /// Can we make this automatically generated when
    /// building the map based on the register/operand
    /// class, utilising PrintAsm trait?
    mnemonic: &'static str,
    /// This should eventually generate the "base" value
    /// for the instruction that the operands are then
    /// OR'd into.
    opcode: Opcode,
    encoding: InstructionEncoding,
}

#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode,
    operands: Vec<Operand>,
    encoding: u8,
    /// Currently unused, but will later
    /// be used to decide which region
    /// to place this
    region: u16,
}

/// This should really be a table that can
/// be looked-up in constant time... perhaps
/// by instantiating every single variant at
/// build time? That way, the key then becomes
/// the final instruction encoding. The challenge
/// will be in figuring out how to (auto)generate
/// the final table at build time, without it
/// being too cumbersome to write/specify.
///
/// Maybe that is something more relevant for
/// disassembly than it is assembling. For now,
/// the focus just needs to be on determining the
/// correct key.
///
/// Furthermore, should the fixed-encoding instructions
/// have their 'mnemonic' updated to reflect the lack of
/// flexibility in operands? How else are you meant to locate
/// the encoding?
static OPCODES: &[OpcodeDesc] = &[
    OpcodeDesc {
        mnemonic: "nop",
        opcode: Opcode::Nop,
        encoding: InstructionEncoding::Fixed { base: 0x0 },
    },
    OpcodeDesc {
        mnemonic: "stop",
        opcode: Opcode::Stop,
        encoding: InstructionEncoding::Fixed { base: 0x1 },
    },
    // LD r, r'
    OpcodeDesc {
        mnemonic: "ld r, r",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::RegReg {
            base: 0b01000000,
            dst_shift: 5,
            src_shift: 2,
        },
    },
    // LD r, n
    OpcodeDesc {
        mnemonic: "ld r, n",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::RegImm8 {
            base: 0b00000110,
            dst_shift: 5,
        },
    },
    // LD r, (HL)
    OpcodeDesc {
        mnemonic: "ld r, $hl",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Reg {
            base: 0b01000110,
            reg_shift: 5,
        },
    },
    // LD (HL), r
    OpcodeDesc {
        mnemonic: "ld $hl, r",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Reg {
            base: 0b01110000,
            reg_shift: 2,
        },
    },
    // LD (HL), n
    OpcodeDesc {
        mnemonic: "ld $hl, n",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm8 { base: 0b00110110 },
    },
    // LD A, (BC)
    OpcodeDesc {
        mnemonic: "ld %a, $bc",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00001010 },
    },
    // LD A, (DE)
    OpcodeDesc {
        mnemonic: "ld %a, $de",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00011010 },
    },
    // LD A, (C)
    OpcodeDesc {
        mnemonic: "ld %a, $c",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b11110010 },
    },
    // LD (C), A
    OpcodeDesc {
        mnemonic: "ld $c, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b11100010 },
    },
    // LD A, (n)
    OpcodeDesc {
        mnemonic: "ld %a, $n",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm8 { base: 0b11110000 },
    },
    // LD (n), A
    OpcodeDesc {
        mnemonic: "ld $n, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm8 { base: 0b11100000 },
    },
    // LD A, (nn)
    OpcodeDesc {
        mnemonic: "ld %a, $nn",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm16 { base: 0b11111010 },
    },
    // LD (nn), A
    OpcodeDesc {
        mnemonic: "ld $nn, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm16 { base: 0b11101010 },
    },
    // LD A, (HL+)
    OpcodeDesc {
        mnemonic: "ldi %a, $hl",
        opcode: Opcode::LoadIncrement,
        encoding: InstructionEncoding::Fixed { base: 0b11101010 },
    },
    // LD A, (HL-)
    OpcodeDesc {
        mnemonic: "ldd %a, $hl",
        opcode: Opcode::LoadDecrement,
        encoding: InstructionEncoding::Fixed { base: 0b00111010 },
    },
    // LD (BC), A
    OpcodeDesc {
        mnemonic: "ld $bc, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00000010 },
    },
    // LD (DE), A
    OpcodeDesc {
        mnemonic: "ld $de, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00010010 },
    },
    // LD (HL+), A
    OpcodeDesc {
        mnemonic: "ldi $hl, %a",
        opcode: Opcode::LoadIncrement,
        encoding: InstructionEncoding::Fixed { base: 0b00100010 },
    },
    // LD (HL-), A
    OpcodeDesc {
        mnemonic: "ldd $hl, %a",
        opcode: Opcode::LoadDecrement,
        encoding: InstructionEncoding::Fixed { base: 0b00110010 },
    },
];

lazy_static::lazy_static! {
static ref OPCODEMAP: HashMap<&'static str, &'static OpcodeDesc> = {
    let mut map: HashMap<&'static str, &'static OpcodeDesc> = HashMap::new();

    for op in OPCODES {
        map.entry(op.mnemonic)
        .or_insert_with(|| op);
    }
    map
};
}

pub fn find_instruction(mnemonic: String, operands: &Vec<Operand>) -> Result<Vec<u8>, String> {
    if let Some(&desc) = OPCODEMAP.get(mnemonic.as_str()) {
        if let Some(enc) = desc.encoding.encode(&operands) {
            return Ok(enc);
        } else {
            return Err(format!(
                "Invalid operands provided for instruction with opcode {:?}",
                desc.opcode,
            )
            .to_owned());
        }
    } else {
        return Err(format!("Could not find instruction for: {}", mnemonic).to_owned());
    }
}
