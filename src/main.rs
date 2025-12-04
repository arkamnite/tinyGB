mod asm_lexer;

fn main() {
    // let input = "ld %a, #5";
    // let input = "%a, %b, %c, %bc, #5, ld, .global";
    let input = "
        .section .data
    str:    .asciz \"Hello, world!\"
        
        .section .text
        .global _start
    
    _start:
        ld a, #5
    ";

    let _tokens = asm_lexer::tokenize(input);
}
