use std::fs;

use crate::{lexer::Token, rom_builder::RomBuilder};
use logos::Logos;

use std::env;

mod asm;
mod gb;
mod lexer;
mod parser;
mod rom_builder;

use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let message = fs::read_to_string(&args[1]).unwrap();
    dbg!(message.clone());

    let mut lexer = Token::lexer(message.as_str());
    let mut parser = Parser::new();
    let instr = parser.parse(&mut lexer).unwrap();

    let mut builder: RomBuilder = RomBuilder::new(rom_builder::CartridgeMapper::Mbc2);

    for value in instr {
        builder.write_value(value).unwrap();
    }

    let rom = builder.build_rom();

    fs::write("test.gb", rom.data.as_slice()).ok();
}
