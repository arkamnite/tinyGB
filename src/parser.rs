use std::collections::HashMap;

use crate::gb::instruction::{Instruction, OpcodeDesc};
use crate::gb::register::Register;
use crate::lexer::Token;
use logos::Span;

type ParserError = (String, Span);
type Result<T> = std::result::Result<T, ParserError>;

pub fn parse(lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
    if let Some(token) = lexer.next() {
        match token {
            Ok(Token::Identifier) => parse_identifier(lexer.slice(), lexer),
            _ => Err(("Parser Error".to_owned(), lexer.span())),
        }
    } else {
        Err((("No tokens provided?").to_owned(), lexer.span()))
    }
}

fn match_operand_description(
    operand_types: Vec<OperandType>,
    options: Vec<&OpcodeDesc>,
) -> Option<&OpcodeDesc> {
    options.into_iter().find(|x| x.operands.eq(&operand_types))
}

// We have already consumed the identifier token
fn parse_identifier(identifier_token: &str, lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
    let span = lexer.span();

    if OPCODEMAP.contains_key(lexer.slice()) {
        return parse_instruction(lexer.slice(), lexer);
    }

    while let Some(token) = lexer.next() {
        // What are the valid identifier types?
        // - All of the OperandType enums
        // - data label
        //
        match token {
            // labels are marked by a colon
            Ok(Token::Colon) => return parse_label(identifier_token, lexer),
            _ => todo!(),
        }
    }
    Err(("Unexpected identifier found here".to_owned(), span))
}

// The colon token has already been consumed
fn parse_label(label_token: &str, lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
    todo!()
}

// Assume we have already consumed the mnemonic for the instruction (instr_token)
// Slight problem. Might need to do more of the parent skeleton to work out who will
// call this function, and therefore when this function stops. In other words, this all
// needs to be structured to use a peekable iterator in order to implement lookahead.
fn parse_instruction(instr_token: &str, lexer: &mut logos::Lexer<'_, Token>) -> Result<Value> {
    let span = lexer.span();
    let mut parsed_operands: Vec<Operand> = Vec::new();
    let mut parsed_operand_types: Vec<OperandType> = Vec::new();

    // We now need to iterate through all future tokens until we get another 'instruction'.
    // Maybe use peekable?
    while let Some(token) = lexer.next() {
        match token {
            // For now, just support registers
            Ok(Token::Register(r)) => {
                parsed_operands.push(Operand::Register(r.clone()));
                // parsed_operand_types.push(OperandType::Register(r));
            }
            // We have arrived at another instruction.
            Ok(Token::Newline) => {
                // We already know that there is a value for this based on who
                // called parse_instruction so we can directly get the value for this key.
                let options = &OPCODEMAP[instr_token];

                // Check if it is valid based on the options we collected earlier
                if let Some(matched_op_desc) =
                    match_operand_description(parsed_operand_types, options.to_vec())
                {
                    let instruction: Instruction = Instruction {
                        opcode: matched_op_desc.opcode,
                        operands: parsed_operands,
                        encoding: matched_op_desc.encoding,
                    };
                    return Ok(Value::Instruction(instruction));
                } else {
                    return Err((("Invalid operands for instruction").to_owned(), span));
                }
            }
            // Other possible operands will be supported later.
            Ok(_) => todo!(),
            _ => todo!(),
        }
    }
    Err(("instruction parsing error".to_owned(), span))
}
