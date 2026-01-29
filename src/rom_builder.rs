use crate::{asm::directives::Section, parser::Value};

const LOGO_BYTES: &[u8] = &[
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

#[derive(Clone)]
pub enum CartridgeMapper {
    RomOnly = 0x00,
    Mbc2 = 0x05,
    Mbc3 = 0x11,
    Mbc5 = 0x19,
}

impl CartridgeMapper {
    pub fn get_max_banks(&self) -> usize {
        match self {
            CartridgeMapper::RomOnly => 2,
            CartridgeMapper::Mbc2 => 16,
            CartridgeMapper::Mbc3 => 128,
            CartridgeMapper::Mbc5 => 512,
        }
    }

    pub fn get_rom_size_header_info(&self) -> u8 {
        match self {
            CartridgeMapper::RomOnly => 0x00,
            CartridgeMapper::Mbc2 => 0x03,
            CartridgeMapper::Mbc3 => 0x06,
            CartridgeMapper::Mbc5 => 0x08,
        }
    }

    pub fn get_ram_size_header_info(&self) -> u8 {
        match self {
            CartridgeMapper::RomOnly => 0,
            CartridgeMapper::Mbc2 => 0,
            CartridgeMapper::Mbc3 => 0,
            CartridgeMapper::Mbc5 => 0,
        }
    }
}

struct Header {
    logo: &'static [u8],             // 0x0104 - 0x0133
    title: [char; 16],               // 0x0134 - 0x0143
    manufacturer: [char; 4],         // 0x013F - 0x0142
    cgb_flag: u8,                    // 0x0143
    publisher: [char; 2],            // 0x0144 - 0x0145
    sgb_flag: u8,                    // 0x0146
    cartridge_type: CartridgeMapper, // 0x0147
    rom_size: u8,                    // 0x0148
    ram_size: u8,                    // 0x0149
    destination_code: bool,          // 0x014A
    old_publisher: u8,               // 0x014B
    mask_rom_number: u8,             // 0x014C
    checksum: u8,                    // 0x014D
    global_checksum: [u8; 2],        // 0x014E - 0x014F
}

impl Default for Header {
    fn default() -> Self {
        Self {
            logo: LOGO_BYTES,
            title: ['0'; 16],
            manufacturer: ['P', 'W', 'R', 'M'],
            // Default to DMG mode
            cgb_flag: 0x80,
            publisher: ['0', '0'],
            sgb_flag: 0x00,
            cartridge_type: CartridgeMapper::RomOnly,
            rom_size: CartridgeMapper::RomOnly.get_rom_size_header_info(),
            ram_size: CartridgeMapper::RomOnly.get_ram_size_header_info(),
            destination_code: true,
            old_publisher: 0x00,
            mask_rom_number: 0x00,
            checksum: Default::default(),
            global_checksum: Default::default(),
        }
    }
}

/// Read-only representation of the final ROM image.
pub struct Rom {
    pub data: Vec<u8>,
}

#[derive(Clone)]
struct RomBank {
    pub bytes: [u8; 0x4000],
    pub cursor: usize,
}

impl Default for RomBank {
    fn default() -> Self {
        Self {
            bytes: [0xff; 0x4000],
            cursor: 0,
        }
    }
}

impl RomBank {}

pub struct RomBuilder {
    // header
    header_info: Header,
    rom_banks: Box<[RomBank]>,
    max_banks: usize,

    entry_cursor: usize,
}

impl RomBuilder {
    pub fn new(mbc: CartridgeMapper) -> RomBuilder {
        let mut default_header = Header::default();
        default_header.cartridge_type = mbc.clone();
        default_header.rom_size = mbc.get_rom_size_header_info();
        default_header.ram_size = mbc.get_ram_size_header_info();
        RomBuilder {
            header_info: default_header,
            rom_banks: vec![RomBank::default(); mbc.get_max_banks()].into_boxed_slice(),
            max_banks: mbc.get_max_banks(),
            entry_cursor: 4,
        }
    }

    fn calculate_header_checksum(&mut self) {
        let mut checksum: u8 = 0;

        for addr in 0x0134..0x014d {
            checksum =
                u8::wrapping_sub(u8::wrapping_sub(checksum, self.rom_banks[0].bytes[addr]), 1);
        }

        self.header_info.checksum = checksum;
        self.rom_banks[0].bytes[0x014d] = self.header_info.checksum;
    }

    pub fn write_value(&mut self, value: Value) -> Result<&mut Self, String> {
        match value {
            Value::Instruction {
                bank_section,
                bytes,
                function,
            } => {
                if bank_section.get_section_index() >= self.max_banks {
                    return Err("Provided section ID is out of bounds!".to_string());
                }

                match bank_section {
                    crate::asm::directives::Section::EntrySection => {
                        // This is the only time we use the built-in cursor for
                        // ROM Bank 0, although we only have a few bytes to use here.
                        if bytes.len() > self.entry_cursor {
                            return Err("Ran out of entry section bytes!".to_string());
                        }

                        for (_, &byte) in bytes.iter().enumerate() {
                            self.rom_banks[0].bytes[Section::EntrySection.get_section_size()
                                - self.entry_cursor
                                + 0x100] = byte;
                            self.entry_cursor = self.entry_cursor - 1;
                        }
                    }
                    crate::asm::directives::Section::RomBank0 => {
                        // Our cursor should always be offset by the end of the header.
                        if bytes.len()
                            > (Section::RomBank0.get_section_size() - self.rom_banks[0].cursor)
                        {
                            return Err("Ran out of bytes for ROM bank 0!".to_string());
                        } else {
                            let bank = &mut self.rom_banks[0];
                            bank.bytes[bank.cursor + 0x150..(bank.cursor + bytes.len() + 0x150)]
                                .clone_from_slice(bytes.as_slice());
                            bank.cursor = bank.cursor + bytes.len();
                        }
                    }
                    _ => {
                        let index = bank_section.get_section_index();
                        let bank = &mut self.rom_banks[index];
                        if bytes.len() > (bank.bytes.len() - bank.cursor) {
                            return Err(
                                format!("Ran out of bytes for ROM bank {}", index).to_string()
                            );
                        } else {
                            bank.bytes[bank.cursor..(bank.cursor + bytes.len())]
                                .clone_from_slice(bytes.as_slice());
                            bank.cursor = bank.cursor + bytes.len();
                        }
                    }
                }
            }
        }
        Ok(self)
    }

    fn char_to_vec(chars: &[char]) -> Vec<u8> {
        chars.into_iter().map(|c| *c as u8).collect()
    }

    fn write_header(&mut self) {
        self.rom_banks[0].bytes[0x0104..0x0134].clone_from_slice(LOGO_BYTES);

        self.rom_banks[0].bytes[0x0134..0x0144]
            .clone_from_slice(RomBuilder::char_to_vec(&self.header_info.title).as_slice());

        self.rom_banks[0].bytes[0x013f..0x0143]
            .clone_from_slice(RomBuilder::char_to_vec(&self.header_info.manufacturer).as_slice());

        self.rom_banks[0].bytes[0x0143] = self.header_info.cgb_flag;

        self.rom_banks[0].bytes[0x0144..0x0146]
            .clone_from_slice(RomBuilder::char_to_vec(&self.header_info.publisher).as_slice());

        self.rom_banks[0].bytes[0x0146] = self.header_info.sgb_flag;

        self.rom_banks[0].bytes[0x0147] = self.header_info.cartridge_type.clone() as u8;
        self.rom_banks[0].bytes[0x0148] = self.header_info.rom_size;
        self.rom_banks[0].bytes[0x0149] = self.header_info.ram_size;
        self.rom_banks[0].bytes[0x014A] = self.header_info.destination_code as u8;
        self.rom_banks[0].bytes[0x014B] = self.header_info.old_publisher;
        self.rom_banks[0].bytes[0x014C] = self.header_info.mask_rom_number;
        self.rom_banks[0].bytes[0x014D] = self.header_info.checksum;
        self.rom_banks[0].bytes[0x014E..0x0150].clone_from_slice(&self.header_info.global_checksum);

        self.calculate_header_checksum();
    }

    pub fn build_rom(&mut self) -> Rom {
        let mut rom_bytes: Vec<u8> = vec![];
        self.write_header();

        for bank in &self.rom_banks {
            for (_, byte) in bank.bytes.iter().enumerate() {
                rom_bytes.push(*byte);
            }
        }

        Rom { data: rom_bytes }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        asm::directives::Section,
        parser::Value,
        rom_builder::{CartridgeMapper, RomBuilder, LOGO_BYTES},
    };

    #[test]
    /// The Game Boy boot ROM expects the 'logo bytes' to be found
    /// in the address range 0x104 - 0x133 in order to boot. This
    /// tests that the final ROM built contains the correct bytes
    /// for this check.
    fn check_logo_dump() {
        let mut builder = RomBuilder::new(CartridgeMapper::RomOnly);
        let rom = builder.build_rom();

        assert_eq!(rom.data.as_slice()[0x0104..0x0134], *LOGO_BYTES);
    }

    #[test]
    fn write_value_entry_ok() {
        let mut builder = RomBuilder::new(CartridgeMapper::RomOnly);
        let result: Result<&mut RomBuilder, String>;
        result = builder.write_value(Value::Instruction {
            bank_section: Section::EntrySection,
            bytes: vec![0x0, 0xc3, 0x50, 0x1],
            function: None,
        });
        assert!(result.is_ok());
        let rom = builder.build_rom();
        assert_eq!(rom.data[0x100..0x104], [0x0, 0xc3, 0x50, 0x1]);
    }

    #[test]
    fn write_value_entry_err() {
        let mut builder = RomBuilder::new(CartridgeMapper::RomOnly);
        let mut result: Result<&mut RomBuilder, String>;
        for _ in 0..Section::EntrySection.get_section_size() {
            result = builder.write_value(Value::Instruction {
                bank_section: Section::EntrySection,
                bytes: vec![0],
                function: None,
            });
            assert!(result.is_ok());
        }
        result = builder.write_value(Value::Instruction {
            bank_section: Section::EntrySection,
            bytes: vec![0],
            function: None,
        });
        assert!(result.is_err());

        builder = RomBuilder::new(CartridgeMapper::RomOnly);
        result = builder.write_value(Value::Instruction {
            bank_section: Section::EntrySection,
            bytes: vec![0; Section::EntrySection.get_section_size() + 1],
            function: None,
        });
        assert!(result.is_err());
    }

    #[test]
    fn write_value_bank_0() {
        let mut builder = RomBuilder::new(CartridgeMapper::RomOnly);
        let mut result: Result<&mut RomBuilder, String>;

        let mut test_bytes: Vec<u8> = Vec::new();
        for i in 0..Section::RomBank0.get_section_size() {
            test_bytes.push(i as u8);
            result = builder.write_value(Value::Instruction {
                bank_section: Section::RomBank0,
                bytes: vec![i as u8],
                function: None,
            });
            assert!(result.is_ok());
        }

        result = builder.write_value(Value::Instruction {
            bank_section: Section::RomBank0,
            bytes: vec![0x0],
            function: None,
        });
        assert!(result.is_err());

        let rom = builder.build_rom();
        assert_eq!(rom.data[0x150..0x4000], *test_bytes.as_slice());

        builder = RomBuilder::new(CartridgeMapper::RomOnly);
        result = builder.write_value(Value::Instruction {
            bank_section: Section::RomBank0,
            bytes: vec![0x0; Section::RomBank0.get_section_size() + 1],
            function: None,
        });
        assert!(result.is_err());
    }

    #[test]
    fn write_value_bank_1() {
        let mut builder = RomBuilder::new(CartridgeMapper::RomOnly);
        let mut result: Result<&mut RomBuilder, String>;

        let mut test_bytes: Vec<u8> = Vec::new();
        for i in 0..Section::RomBank1.get_section_size() {
            test_bytes.push(i as u8);
            result = builder.write_value(Value::Instruction {
                bank_section: Section::RomBank1,
                bytes: vec![i as u8],
                function: None,
            });
            assert!(result.is_ok());
        }

        result = builder.write_value(Value::Instruction {
            bank_section: Section::RomBank1,
            bytes: vec![0x0],
            function: None,
        });
        assert!(result.is_err());

        let rom = builder.build_rom();
        assert_eq!(rom.data[0x4000..0x8000], *test_bytes.as_slice());

        builder = RomBuilder::new(CartridgeMapper::RomOnly);
        result = builder.write_value(Value::Instruction {
            bank_section: Section::RomBank1,
            bytes: vec![0x0; Section::RomBank1.get_section_size() + 1],
            function: None,
        });
        assert!(result.is_err());
    }
}
