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
    let input = "
    .section .entry
        nop
        jp $0150

    .section .data 0
        ld %a, $c
        ld $c, %a
        ld %a, %c
    ";

    let mut lexer = Token::lexer(input);
    let mut parser = Parser::new();
    let instr = parser.parse(&mut lexer).unwrap();

    let mut builder: RomBuilder = RomBuilder::new(rom_builder::CartridgeMapper::Mbc2);

    for value in instr {
        builder.write_value(value).unwrap();
    }

    let rom = builder.build_rom();

    fs::write("test.gb", rom.data.as_slice()).ok();
}
