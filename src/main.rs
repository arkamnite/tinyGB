mod asm_lexer;
mod parser;

fn main() {
    let input = "
        .section .data
    str:    .asciz \"Hello, world!\"
        
        .section .text
        .global _start
    
    _start:
        ld %a, #5
    ";
    let input = "
        ld %a, #5
    ";

    let _tokens = asm_lexer::tokenize(input);
}
