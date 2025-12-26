use std::collections::HashMap;

use crate::gb::instruction::{
    find_instruction, match_opcode, Instruction, Opcode, OpcodeDesc, Operand,
};
use crate::gb::register::{Register, Register8};
use crate::lexer::Token;
use logos::Span;

type ParserError = (String, Span);
type Result<T> = std::result::Result<T, ParserError>;

pub enum Value {
    Instruction(Vec<u8>),
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
    if let Some(opcode) = match_opcode(identifier_token.to_string()) {
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

fn parse_instruction(opcode: Opcode, lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
    let mut parsed_operands: Vec<Operand> = Vec::new();
    let span = lexer.span();
    let mut parsed_mnemonic: String = opcode.to_string();

    while let Some(token) = lexer.next() {
        match token {
            Ok(Token::Newline) => {
                // Arrived at a new instruction, prepare to return something.

                // Check the table?
                if let Some(instr_vec) =
                    find_instruction(parsed_mnemonic.to_string(), &parsed_operands).ok()
                {
                    return Ok(Value::Instruction(instr_vec));
                }
            }
            Ok(Token::Identifier) => {
                // Another instruction?
                if let Some(_) = match_opcode(lexer.slice().to_string()) {
                    return Err(("unexpected instruction identifier in line".to_owned(), span));
                }
            }
            Ok(Token::Register(r)) => {
                let operand = Operand::Register8(Register8::try_from(r).unwrap());
                parsed_mnemonic.push_str(" ");
                parsed_mnemonic.push_str(operand.gen_placeholder().as_str());
                parsed_operands.push(operand);
            }
            Ok(Token::RegisterPtr(rp)) => {}
            Ok(Token::Integer(i)) => {
                // Check if this is wider than u8, demote if not.
                // Also check for signed range based on previous
                // instruction?
            }
            Ok(Token::IntegerPointer(ip)) => {
                // Check if wider than u8, demote if not.
            }
            Ok(Token::Comma) => {
                parsed_mnemonic.push_str(", ");
            }
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
