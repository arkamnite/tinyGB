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
        function: Option<String>,
        bytes: Vec<u8>,
    },
}

struct Function {
    scope: FunctionScope,
    defined: bool,
}

#[derive(Clone, Copy)]
enum FunctionScope {
    Global,
}

pub struct Parser {
    current_section: Section,
    current_function: Option<String>,
    functions: HashMap<String, Function>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            current_section: Section::EntrySection,
            current_function: None,
            functions: HashMap::new(),
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
                            Directives::GlobalLabel => self.parse_directive_global(lexer)?,
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

    fn parse_directive_global(&mut self, lexer: &mut logos::Lexer<'_, Token>) -> Result<()> {
        // Expect to parse an identifier for this label.
        if let Some(t1) = lexer.next() {
            match t1 {
                Ok(Token::Identifier) => {
                    let identifier = lexer.slice();

                    if let Some(t2) = lexer.next() {
                        match t2 {
                            Ok(Token::Newline) => {
                                match self.functions.insert(
                                    identifier.to_string(),
                                    Function {
                                        scope: FunctionScope::Global,
                                        defined: false,
                                    },
                                ) {
                                    Some(_) => Err(".global function already defined!".to_string()),
                                    None => Ok(()),
                                }
                            }
                            _ => {
                                Err(".global directives expect a newline after the label"
                                    .to_string())
                            }
                        }
                    } else {
                        Err(".global directives expect a newline after the label".to_string())
                    }
                }
                Ok(_) => Err(".global directive expects an identifier!".to_string()),
                Err(e) => Err(e),
            }
        } else {
            Err(".global directive expects an identifier!".to_string())
        }
        // Expect to then parse a colon
    }

    fn parse_identifier(
        &mut self,
        identifier_token: &str,
        lexer: &mut logos::Lexer<'_, Token>,
    ) -> Result<Value> {
        // check if this is a function definition
        if let Some(function) = self.functions.get(lexer.slice()) {
            if let Some(token) = lexer.next() {
                match token {
                    Ok(Token::Colon) => {
                        if function.defined {
                            return Err(
                                format!("function {} redefined!", lexer.slice()).to_string()
                            );
                        } else {
                            self.current_function = Some(lexer.slice().to_string());
                        }
                    }
                    _ => {}
                }
            }
        }

        // match the current token to an instruction
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
                        function: self.current_function.clone(),
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
                    // TODO: rombuilder's current implementation means that we have to handle
                    // relocations within the parser; rombuilder only accepts a series of bytes
                    // for each instruction, and then writes them to memory based on the current
                    // cursor position within the rombuilder state. This makes it impossible to
                    // pass labels to rombuilder, as it only knows how to consume bytes and changing
                    // this would mean changing the entire definition of a Value::Instruction as well
                    // as the Instruction implementation.
                    //
                    // This means that we need to keep track of the cursor in the parser now- this
                    // is doable since we are already encoding the section each instruction belongs
                    // to- and then add the cursor data to each Value::Instruction.
                    //
                    // Value::Instruction therefore needs to contain a cursor for the given instruction,
                    // and rombuilder shouldn't keep track of the cursor for a given instruction. "Section"
                    // limits may not always exist on platforms other than Game Boy, but when they do,
                    // they can be enforced in the parser.
                    //
                    // Another thing to bear in mind: a 'global' function can only live in certain sections
                    // (i.e. bank 0), and if the parser is going to handle the locations of each label,
                    // it should have an organised way of arranging data on a per-section basis.
                    // Box<[FunctionData]> or similar might be good.
                    todo!()
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
                    // We will eventually need to check for signed values too.
                    match opcode {
                        GeneralOpcode::Add
                        | GeneralOpcode::AddCarry
                        | GeneralOpcode::Sub
                        | GeneralOpcode::SubCarry
                        | GeneralOpcode::And
                        | GeneralOpcode::Compare
                        | GeneralOpcode::Or
                        | GeneralOpcode::Xor => match u8::try_from(i) {
                            // Once we add 16-bit arithmetic, we are going to have to check whether
                            // or not the previous operand was a 16-bit register pair as the mnemonic
                            // is the same for 8-bit and 16-bit arithmetic - there is no point creating
                            // another generic opcode for this.
                            Ok(int8) => parsed_operands.push(Operand::Imm8(int8)),
                            Err(_) => return Err(
                                "Only 8-bit immediate values can be used with this instruction!"
                                    .to_string(),
                            ),
                        },
                        _ => parsed_operands.push(Operand::Imm16(i)),
                    }
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
