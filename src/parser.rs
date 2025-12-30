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
            Ok(Token::Newline) => {}
            Ok(Token::Identifier) => {
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
    let mut parsed_operands: Vec<Operand> = vec![];
    let span = lexer.span();

    while let Some(token) = lexer.next() {
        match token {
            Ok(Token::Newline) => {
                if let Some(instr_vec) = find_instruction(opcode.clone(), &parsed_operands) {
                    // Encode the value now
                    return Ok(Value::Instruction(
                        instr_vec.encode(&parsed_operands.as_slice())?,
                    ));
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
                let try_operand = Operand::try_from(Token::Register(r)).ok();
                match try_operand {
                    Some(op) => parsed_operands.push(op),
                    None => {
                        return Err((("Invalid operand provided!").to_string(), span));
                    }
                }
            }
            Ok(Token::RegisterPtr(p)) => {
                let try_operand = Operand::try_from(Token::RegisterPtr(p)).ok();
                match try_operand {
                    Some(op) => parsed_operands.push(op),
                    None => {
                        return Err((("Invalid operand provided!").to_string(), span));
                    }
                }
            }
            Ok(Token::Register16(r)) => {
                let try_operand = Operand::try_from(Token::Register16(r)).ok();
                match try_operand {
                    Some(op) => parsed_operands.push(op),
                    None => {
                        return Err((("Invalid operand provided!").to_string(), span));
                    }
                }
            }
            Ok(Token::RegisterPtr16(p)) => {
                let try_operand = Operand::try_from(Token::RegisterPtr16(p)).ok();
                match try_operand {
                    Some(op) => parsed_operands.push(op),
                    None => {
                        return Err((("Invalid operand provided!").to_string(), span));
                    }
                }
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
