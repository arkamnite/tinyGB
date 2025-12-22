use core::panic;

use crate::gb::register::Register;
use logos::{Lexer, Logos};

fn register_callback(lex: &mut Lexer<Token>) -> Option<Register> {
    let slice = lex.slice();
    let name = slice.get(1..)?;

    match name {
        "a" => Some(Register::A),
        "b" => Some(Register::B),
        "c" => Some(Register::C),
        "d" => Some(Register::D),
        "e" => Some(Register::E),
        "h" => Some(Register::H),
        "l" => Some(Register::L),
        "bc" => Some(Register::BC),
        "de" => Some(Register::DE),
        "hl" => Some(Register::HL),
        "pc" => Some(Register::PC),
        "sp" => Some(Register::SP),
        _ => None,
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t]+")]
#[logos(error = String)]
pub enum Token {
    #[token("\n")]
    Newline,

    #[regex(r"\.(\w)+")]
    Directive,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token(".")]
    Period,

    #[token("-")]
    Dash,

    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    // Excludes //, delimiters, and '"'
    #[regex(r#""([^"\\]|\\.)*""#)]
    StringLiteral,

    #[regex(r"\w+")]
    Identifier,

    #[regex(r"%[a-z][a-z]?", register_callback)]
    Register(Register),

    // Need to include signed 8-bit numbers? Might need several enums for this.
    #[regex("#[0-9]+", |lex| lex.slice()[1..].parse::<u8>().unwrap())]
    Integer(u8),
}

pub fn tokenize(src: &str) {
    println!("{}", src);
    for result in Token::lexer(src) {
        match result {
            Ok(token) => println!("{:#?}", token),
            Err(e) => panic!("A lexer error occurred: {}", e),
        }
    }
}
