/* Operand Description */

use std::{collections::HashMap, fmt, u8};

use strum_macros::EnumDiscriminants;

use crate::gb::{
    asm::Encodable,
    register::{
        ARegister, CRegisterPtr, HLRegisterPtr, PushPopRegister16, Register16, Register8,
        RegisterPtr16, RegisterPtr8,
    },
};

#[derive(Debug, Clone, EnumDiscriminants)]
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

pub struct OperandDesc {
    operand_type: OperandDiscriminants,
    shift: u8,
}

pub struct InstructionDesc {
    opcode: Opcode,
    base: u8,
    input_desc: Option<OperandDesc>,
    output_desc: Option<OperandDesc>,
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

#[derive(Debug, Clone, Copy)]
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

/* Instructions */
pub struct OpcodeEncDesc {
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
///
/// - see below for the answer to this question a.k.a. the
///   issue with inflexible mnemonics for the fixed-encoding
///   instructions.
///
/// Current problem: inconsistent 'mnemonic' scheme.
/// E.g. take 'ld %a, $c'; this is ambiguous with 'ld r, $r' which
/// is actually used for lookup. If this 'ld r, $r' instruction
/// actually exists, there should be a specific opcode (rather than)
/// just `Opcode::Load`, and this should then be used to enforce the
/// classes of operand it accepts. It should be possible to check during
/// parsing whether an incorrect operand has been parsed based on the
/// opcode.
///
/// To solve the above example, there would therefore be two new opcodes
/// created- `Opcode::LoadRdPtr` and `Opcode::LoadPtrRr`- and each opcode
/// must specify a valid type for 'ins' and 'outs'.
///
/// What really needs to happen here is some concept of register
/// classes, which thankfully, have already been defined for the
/// most part.
static OPCODES_OLD: &[OpcodeEncDesc] = &[
    OpcodeEncDesc {
        mnemonic: "nop",
        opcode: Opcode::Nop,
        encoding: InstructionEncoding::Fixed { base: 0x0 },
    },
    OpcodeEncDesc {
        mnemonic: "stop",
        opcode: Opcode::Stop,
        encoding: InstructionEncoding::Fixed { base: 0x1 },
    },
    // LD r, r'
    OpcodeEncDesc {
        mnemonic: "ld r, r",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::RegReg {
            base: 0b01000000,
            dst_shift: 5,
            src_shift: 2,
        },
    },
    // LD r, n
    OpcodeEncDesc {
        mnemonic: "ld r, n",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::RegImm8 {
            base: 0b00000110,
            dst_shift: 5,
        },
    },
    // LD r, (HL)
    OpcodeEncDesc {
        mnemonic: "ld r, $hl",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Reg {
            base: 0b01000110,
            reg_shift: 5,
        },
    },
    // LD (HL), r
    OpcodeEncDesc {
        mnemonic: "ld $hl, r",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Reg {
            base: 0b01110000,
            reg_shift: 2,
        },
    },
    // LD (HL), n
    OpcodeEncDesc {
        mnemonic: "ld $hl, n",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm8 { base: 0b00110110 },
    },
    // LD A, (BC)
    OpcodeEncDesc {
        mnemonic: "ld %a, $bc",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00001010 },
    },
    // LD A, (DE)
    OpcodeEncDesc {
        mnemonic: "ld %a, $de",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00011010 },
    },
    // LD A, (C)
    OpcodeEncDesc {
        mnemonic: "ld %a, $c",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b11110010 },
    },
    // LD (C), A
    OpcodeEncDesc {
        mnemonic: "ld $c, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b11100010 },
    },
    // LD A, (n)
    OpcodeEncDesc {
        mnemonic: "ld %a, $n",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm8 { base: 0b11110000 },
    },
    // LD (n), A
    OpcodeEncDesc {
        mnemonic: "ld $n, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm8 { base: 0b11100000 },
    },
    // LD A, (nn)
    OpcodeEncDesc {
        mnemonic: "ld %a, $nn",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm16 { base: 0b11111010 },
    },
    // LD (nn), A
    OpcodeEncDesc {
        mnemonic: "ld $nn, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Imm16 { base: 0b11101010 },
    },
    // LD A, (HL+)
    OpcodeEncDesc {
        mnemonic: "ldi %a, $hl",
        opcode: Opcode::LoadIncrement,
        encoding: InstructionEncoding::Fixed { base: 0b11101010 },
    },
    // LD A, (HL-)
    OpcodeEncDesc {
        mnemonic: "ldd %a, $hl",
        opcode: Opcode::LoadDecrement,
        encoding: InstructionEncoding::Fixed { base: 0b00111010 },
    },
    // LD (BC), A
    OpcodeEncDesc {
        mnemonic: "ld $bc, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00000010 },
    },
    // LD (DE), A
    OpcodeEncDesc {
        mnemonic: "ld $de, %a",
        opcode: Opcode::Load,
        encoding: InstructionEncoding::Fixed { base: 0b00010010 },
    },
    // LD (HL+), A
    OpcodeEncDesc {
        mnemonic: "ldi $hl, %a",
        opcode: Opcode::LoadIncrement,
        encoding: InstructionEncoding::Fixed { base: 0b00100010 },
    },
    // LD (HL-), A
    OpcodeEncDesc {
        mnemonic: "ldd $hl, %a",
        opcode: Opcode::LoadDecrement,
        encoding: InstructionEncoding::Fixed { base: 0b00110010 },
    },
];

lazy_static::lazy_static! {
static ref OPCODEMAP_OLD: HashMap<&'static str, &'static OpcodeEncDesc> = {
    let mut map: HashMap<&'static str, &'static OpcodeEncDesc> = HashMap::new();

    for op in OPCODES_OLD {
        map.entry(op.mnemonic)
        .or_insert_with(|| op);
    }
    map
};
}

pub fn find_instruction(mnemonic: &String, operands: &Vec<Operand>) -> Result<Vec<u8>, String> {
    if let Some(&desc) = OPCODEMAP_OLD.get(mnemonic.as_str()) {
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
