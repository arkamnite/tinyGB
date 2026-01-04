use std::fs;

use crate::{lexer::Token, rom_builder::RomBuilder};
use logos::Logos;

mod asm;
mod gb;
mod lexer;
mod parser;
mod rom_builder;

use parser::Parser;

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
    .section .entry
        nop
        jp $0150

    .section .data 0
        ld %a, %c
        ld %a, $c
        ld $c, %a
        ";
    // ld %a, #5
    // let _tokens = lexer::tokenize(input);
    println!("==================== Parser ====================");
    let mut lexer = Token::lexer(input);
    let mut parser = Parser::new();
    let _instr = parser.parse(&mut lexer).unwrap();

    let mut builder = RomBuilder::new();
    let rom = builder.build_rom();

    fs::write("test.gb", rom.data.as_slice()).ok();

    println!("==================== /Parser ===================");
}
