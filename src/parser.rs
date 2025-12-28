use std::collections::HashMap;

use crate::gb::instruction::{find_instruction, Opcode, Operand, OperandDiscriminants};
use crate::gb::register::{Register, Register8, RegisterPtr8};
use crate::lexer::Token;
use logos::Span;
use strum_macros::{AsRefStr, IntoStaticStr};

pub type ParserError = (String, Span);
type Result<T> = std::result::Result<T, ParserError>;

/// General opcode variants which have a 1:1 relationship
/// with all the different identifier tokens that can be
/// used to represent instructions.

#[derive(IntoStaticStr, AsRefStr, Clone, Eq, PartialEq)]
pub enum GeneralOpcode {
    #[strum(serialize = "nop")]
    Nop,
    #[strum(serialize = "stop")]
    Stop,
    #[strum(serialize = "ld")]
    Load,
    #[strum(serialize = "ldi")]
    LoadIncrement,
    #[strum(serialize = "ldd")]
    LoadDecrement,
}

pub enum Value {
    Instruction(Vec<u8>),
}

fn match_general_opcode(token: &str) -> Option<GeneralOpcode> {
    match token {
        val if val == GeneralOpcode::Nop.as_ref() => Some(GeneralOpcode::Nop),
        val if val == GeneralOpcode::Stop.as_ref() => Some(GeneralOpcode::Stop),
        val if val == GeneralOpcode::Load.as_ref() => Some(GeneralOpcode::Load),
        val if val == GeneralOpcode::LoadIncrement.as_ref() => Some(GeneralOpcode::LoadIncrement),
        val if val == GeneralOpcode::LoadDecrement.as_ref() => Some(GeneralOpcode::LoadDecrement),
        _ => None,
    }
}

pub fn parse(lexer: &mut logos::Lexer<'_, Token>) -> Result<Vec<Value>> {
    let mut values: Vec<Value> = Vec::new();
    while let Some(token) = lexer.next() {
        match token {
            Ok(Token::Newline) => {
                println!("Parser: Newline");
            }
            Ok(Token::Identifier) => {
                println!("Parser: Identifier");
                values.push(parse_identifier(lexer.slice(), lexer)?);
            }
            _ => {
                return Err(("Parser Error".to_owned(), lexer.span()));
            }
        };
    }
    if values.is_empty() {
        Err((("No tokens provided?").to_owned(), lexer.span()))
    } else {
        return Ok(values);
    }
}

// fn match_operand_description(
//     operand_types: Vec<OperandType>,
//     options: Vec<&OpcodeDesc>,
// ) -> Option<&OpcodeDesc> {
//     options.into_iter().find(|x| x.operands.eq(&operand_types))
// }

// We have already consumed the identifier token
fn parse_identifier(identifier_token: &str, lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
    let span = lexer.span();

    // match the current token
    if let Some(opcode) = match_general_opcode(identifier_token) {
        return parse_instruction(opcode, lexer);
    }

    while let Some(token) = lexer.next() {
        match token {
            // labels are marked by a colon
            // Ok(Token::Colon) => return parse_label(identifier_token, lexer),
            _ => todo!(),
        }
    }
    Err(("Unexpected identifier found here".to_owned(), span))
}

fn parse_instruction(opcode: GeneralOpcode, lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
    let mut parsed_input_operand: Option<Operand> = None;
    let mut parsed_output_operand: Option<Operand> = None;
    let span = lexer.span();

    while let Some(token) = lexer.next() {
        match token {
            Ok(Token::Newline) => {
                if let Some(instr_vec) = find_instruction(
                    opcode.clone(),
                    parsed_input_operand.map_or(None, |opt| Some(OperandDiscriminants::from(opt))),
                    parsed_output_operand.map_or(None, |opt| Some(OperandDiscriminants::from(opt))),
                ) {
                    // Encode the value now
                    return Ok(Value::Instruction(instr_vec.encode()?));
                } else {
                    return Err((
                        format!("unable to match instruction: {}", opcode.as_ref()).to_owned(),
                        span,
                    ));
                }
            }
            Ok(Token::Identifier) => {
                // Another instruction?
                if let Some(_) = match_general_opcode(lexer.slice()) {
                    return Err(("unexpected instruction identifier in line".to_owned(), span));
                }
            }
            Ok(Token::Register(r)) => {
                todo!()
            }
            Ok(Token::RegisterPtr(rp)) => {
                // We need to rewrite the 'from' logic so that we can match to the correct register
                // class.
                let operand = Operand::RegisterPtr8(RegisterPtr8::try_from(rp).unwrap());
                todo!()
            }
            Ok(Token::Integer(i)) => {
                // Check if this is wider than u8, demote if not.
                // Also check for signed range based on previous
                // instruction?
            }
            Ok(Token::IntegerPointer(ip)) => {
                // Check if wider than u8, demote if not.

                // specific range + reg combo should
                // be matched to Opcode::LoadImmPtr8Rr
                // etc.
            }
            Ok(Token::Comma) => {}
            Ok(_) => todo!(),
            _ => todo!(),
        }
    }
    Err(("unexpected end of instruction".to_owned(), span))
}

// Assume we have already consumed the mnemonic for the instruction (instr_token)
// Slight problem. Might need to do more of the parent skeleton to work out who will
// call this function, and therefore when this function stops. In other words, this all
// needs to be structured to use a peekable iterator in order to implement lookahead.
// fn parse_instruction(instr_token: &str, lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
//     let span = lexer.span();
//     let mut parsed_operands: Vec<Operand> = Vec::new();
//     let mut parsed_operand_types: Vec<OperandType> = Vec::new();

//     // We now need to iterate through all future tokens until we get another 'instruction'.
//     // Maybe use peekable?
//     while let Some(token) = lexer.next() {
//         match token {
//             // For now, just support registers
//             Ok(Token::Register(r)) => {
//                 parsed_operands.push(Operand::Register(r.clone()));
//                 // parsed_operand_types.push(OperandType::Register(r));
//             }
//             // We have arrived at another instruction.
//             Ok(Token::Newline) => {
//                 // We already know that there is a value for this based on who
//                 // called parse_instruction so we can directly get the value for this key.
//                 let options = &OPCODEMAP[instr_token];

//                 // Check if it is valid based on the options we collected earlier
//                 if let Some(matched_op_desc) =
//                     match_operand_description(parsed_operand_types, options.to_vec())
//                 {
//                     let instruction: Instruction = Instruction {
//                         opcode: matched_op_desc.opcode,
//                         operands: parsed_operands,
//                         encoding: matched_op_desc.encoding,
//                     };
//                     return Ok(Value::Instruction(instruction));
//                 } else {
//                     return Err((("Invalid operands for instruction").to_owned(), span));
//                 }
//             }
//             // Other possible operands will be supported later.
//             Ok(_) => todo!(),
//             _ => todo!(),
//         }
//     }
//     Err(("instruction parsing error".to_owned(), span))
// }
