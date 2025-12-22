pub trait Encodable {
    fn encode(&self) -> String;
}

pub trait PrintAsm {
    fn print_asm(&self) -> String;
}

pub trait Emit {
    fn emit(&self) -> Option<Vec<u8>>;
}
