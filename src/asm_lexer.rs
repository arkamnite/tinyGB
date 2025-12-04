use core::panic;

use logos::{Lexer, Logos};

#[derive(Debug, PartialEq)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    BC,
    DE,
    HL,
}

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
        _ => None,
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n]+")]
#[logos(error = String)]
pub enum Token {
    // Directives
    // #[regex(r"\.[a-z]+")]
    // Directive,
    // Labels
    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token(".")]
    Period,

    #[token("\"")]
    DoubleQuote,

    #[token("!")]
    ExclamationMark,

    #[token("=")]
    Equals,

    #[regex(r"\w+")]
    Identifier,

    // Match to an enum
    // #[regex(r"[a-z]+")]
    // Instruction,

    // Need to match this to a specific register?
    #[regex(r"%[a-z][a-z]?", register_callback)]
    Register(Register),

    // Change this to include a specific
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
