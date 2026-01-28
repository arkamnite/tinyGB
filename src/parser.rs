use core::slice;
use std::collections::HashMap;

use crate::asm::directives::{match_directive, Directives, Section};
use crate::gb::instruction::{find_instruction, Opcode, Operand, OperandDiscriminants};
use crate::gb::register::{Register, Register8, RegisterPtr8};
use crate::lexer::Token;
use logos::{Lexer, Span};
use strum_macros::{AsRefStr, IntoStaticStr};

pub type ParserError = String;
type Result<T> = std::result::Result<T, ParserError>;

/// General opcode variants which have a 1:1 relationship
/// with all the different identifier tokens that can be
/// used to represent instructions.

#[derive(Debug, IntoStaticStr, AsRefStr, Clone, Eq, PartialEq)]
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
    #[strum(serialize = "jp")]
    Jump,
    #[strum(serialize = "add")]
    Add,
    #[strum(serialize = "adc")]
    AddCarry,
    #[strum(serialize = "sub")]
    Sub,
    #[strum(serialize = "sbc")]
    SubCarry,
    #[strum(serialize = "and")]
    And,
    #[strum(serialize = "or")]
    Or,
    #[strum(serialize = "xor")]
    Xor,
    #[strum(serialize = "cp")]
    Compare,
    #[strum(serialize = "inc")]
    Increment,
    #[strum(serialize = "dec")]
    Decrement,
}

pub enum Value {
    Instruction {
        bank_section: Section,
        bytes: Vec<u8>,
    },
}

pub struct Parser {
    current_section: Section,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            current_section: Section::EntrySection,
        }
    }

    fn match_general_opcode(token: &str) -> Option<GeneralOpcode> {
        match token {
            val if val == GeneralOpcode::Nop.as_ref() => Some(GeneralOpcode::Nop),
            val if val == GeneralOpcode::Stop.as_ref() => Some(GeneralOpcode::Stop),
            val if val == GeneralOpcode::Load.as_ref() => Some(GeneralOpcode::Load),
            val if val == GeneralOpcode::LoadIncrement.as_ref() => {
                Some(GeneralOpcode::LoadIncrement)
            }
            val if val == GeneralOpcode::LoadDecrement.as_ref() => {
                Some(GeneralOpcode::LoadDecrement)
            }
            val if val == GeneralOpcode::Add.as_ref() => Some(GeneralOpcode::Add),
            val if val == GeneralOpcode::AddCarry.as_ref() => Some(GeneralOpcode::AddCarry),
            val if val == GeneralOpcode::Sub.as_ref() => Some(GeneralOpcode::Sub),
            val if val == GeneralOpcode::SubCarry.as_ref() => Some(GeneralOpcode::SubCarry),
            val if val == GeneralOpcode::And.as_ref() => Some(GeneralOpcode::And),
            val if val == GeneralOpcode::Or.as_ref() => Some(GeneralOpcode::Or),
            val if val == GeneralOpcode::Xor.as_ref() => Some(GeneralOpcode::Xor),
            val if val == GeneralOpcode::Compare.as_ref() => Some(GeneralOpcode::Compare),
            val if val == GeneralOpcode::Increment.as_ref() => Some(GeneralOpcode::Increment),
            val if val == GeneralOpcode::Decrement.as_ref() => Some(GeneralOpcode::Decrement),
            val if val == GeneralOpcode::Jump.as_ref() => Some(GeneralOpcode::Jump),
            _ => None,
        }
    }

    pub fn parse(&mut self, lexer: &mut logos::Lexer<'_, Token>) -> Result<Vec<Value>> {
        let mut values: Vec<Value> = Vec::new();
        while let Some(token) = lexer.next() {
            match token {
                Ok(Token::Newline) => {}
                Ok(Token::Directive) => {
                    if let Some(directive) = match_directive(lexer.slice()) {
                        match directive {
                            Directives::Section => self.parse_directive_section(lexer)?,
                            Directives::Entry => {
                                return Err(("Unexpected .entry section").to_string())
                            }
                            Directives::Data => {
                                return Err(("Unexpected .data section").to_string())
                            }
                        }
                    } else {
                        return Err(
                            (format!("Unsupported directive: {}", lexer.slice()).to_string())
                        );
                    }
                }
                Ok(Token::Identifier) => {
                    values.push(self.parse_identifier(lexer.slice(), lexer)?);
                }
                _ => {
                    return Err("Parser Error".to_owned());
                }
            };
        }
        if values.is_empty() {
            Err(("No tokens provided?").to_owned())
        } else {
            return Ok(values);
        }
    }

    fn parse_directive_section(&mut self, lexer: &mut logos::Lexer<'_, Token>) -> Result<()> {
        let mut expects_data_id: bool = false;
        while let Some(token) = lexer.next() {
            match token {
                Ok(Token::Newline) => {
                    if expects_data_id {
                        return Err(("Unterminated .section directive?").to_string());
                    }
                }
                Ok(Token::Identifier) => {
                    if expects_data_id {
                        match lexer.slice().parse::<u16>() {
                            Ok(id) => match id {
                                0 => {
                                    self.current_section = Section::RomBank0;
                                    return Ok(());
                                }
                                1 => {
                                    self.current_section = Section::RomBank1;
                                    return Ok(());
                                }
                                _ => {
                                    return Err(format!(
                                        "Unsupported bank ID for .data directive: {}",
                                        id
                                    )
                                    .to_string());
                                }
                            },
                            Err(_) => {
                                return Err(format!(
                                    "Unsupported bank ID for .data directive: {}",
                                    lexer.slice()
                                )
                                .to_string());
                            }
                        }
                    } else {
                        return Err(
                            ("Unexpected identifier following .data directive!").to_string()
                        );
                    }
                }
                Ok(Token::Directive) => match match_directive(lexer.slice()) {
                    Some(Directives::Entry) => {
                        self.current_section = Section::EntrySection;
                        return Ok(());
                    }
                    Some(Directives::Data) => {
                        expects_data_id = true;
                    }
                    _ => {
                        return Err(
                            ("Expected one of .entry or .data following .section!").to_string()
                        )
                    }
                },
                _ => {}
            }
        }
        return Ok(());
    }

    fn parse_identifier(
        &mut self,
        identifier_token: &str,
        lexer: &mut logos::Lexer<'_, Token>,
    ) -> Result<Value> {
        let span = lexer.span();

        // match the current token
        if let Some(opcode) = Parser::match_general_opcode(identifier_token) {
            return self.parse_instruction(opcode, lexer);
        }

        while let Some(token) = lexer.next() {
            match token {
                // labels are marked by a colon
                // Ok(Token::Colon) => return parse_label(identifier_token, lexer),
                _ => todo!(),
            }
        }
        Err("Unexpected identifier found here".to_owned())
    }

    fn match_instruction(
        &mut self,
        opcode: GeneralOpcode,
        operands: &Vec<Operand>,
    ) -> Result<Value> {
        if let Some(instr_vec) = find_instruction(opcode.clone(), &operands) {
            match instr_vec.encode(&operands.as_slice()) {
                Ok(bytes) => {
                    return Ok(Value::Instruction {
                        bank_section: self.current_section.clone(),
                        bytes,
                    });
                }
                Err(e) => {
                    return Err(format!("{}, opcode: {:?}", e.0, e.1));
                }
            }
        } else {
            return Err(format!("unable to match instruction: {}", opcode.as_ref()).to_owned());
        }
    }

    fn parse_instruction(
        &mut self,
        opcode: GeneralOpcode,
        lexer: &mut logos::Lexer<'_, Token>,
    ) -> Result<Value> {
        let mut parsed_operands: Vec<Operand> = vec![];

        while let Some(token) = lexer.next() {
            dbg!(lexer.slice());
            match token {
                Ok(Token::Newline | Token::Eof) => {
                    return self.match_instruction(opcode, &parsed_operands)
                }
                Ok(Token::Identifier) => {
                    // Another instruction?
                    if let Some(_) = Parser::match_general_opcode(lexer.slice()) {
                        return Err("unexpected instruction identifier in line".to_owned());
                    }
                }
                Ok(
                    Token::Register(_)
                    | Token::Register16(_)
                    | Token::RegisterPtr(_)
                    | Token::RegisterPtr16(_),
                ) => match token {
                    Ok(t) => {
                        let try_operand = Operand::try_from(t);
                        match try_operand {
                            Ok(op) => parsed_operands.push(op),
                            Err(op_err) => return Err(op_err.0.to_string()),
                        }
                    }
                    Err(token_error) => {
                        return Err(token_error);
                    }
                },
                Ok(Token::Integer(i)) => {
                    // Check if this is wider than u8, demote if not.
                    // Also check for signed range based on previous
                    // instruction?
                    todo!()
                }
                Ok(Token::IntegerPointer(ip)) => {
                    // Check if wider than u8, demote if not.
                    match opcode {
                        GeneralOpcode::Jump => {
                            if let Some(op) = Operand::try_from(Token::IntegerPointer(ip)).ok() {
                                parsed_operands.push(op);
                            } else {
                                return Err(("Invalid operand provided!").to_string());
                            }
                        }
                        _ => todo!(),
                    }

                    // specific range + reg combo should
                    // be matched to Opcode::LoadImmPtr8Rr
                    // etc.
                }
                Ok(Token::Comma) => {}
                Ok(_) => todo!(),
                _ => todo!(),
            }
        }
        self.match_instruction(opcode, &parsed_operands)
    }
}
