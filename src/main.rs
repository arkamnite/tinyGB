use crate::lexer::Token;
use logos::Logos;

mod gb;
mod lexer;
mod parser;

fn main() {
    // let input = "
    //     .section .data
    // str:    .asciz \"Hello, world!\"

    //     .section .text
    //     .global _start

    // _start:
    //     ld %a, #5
    // ";
    let input = "
        ld %a, %c
        ";
    // ld %a, #5

    println!("==================== Lexer =====================");
    // let _tokens = lexer::tokenize(input);
    println!("==================== /Lexer ====================");
    println!("==================== Parser ====================");
    let mut lexer = Token::lexer(input);
    let _instr = parser::parse(&mut lexer).unwrap();
    println!("==================== /Parser ===================");
}
