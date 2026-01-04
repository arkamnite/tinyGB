use crate::parser::Value;

const LOGO_BYTES: &[u8] = &[
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

#[derive(Clone)]
enum CartridgeMapper {
    RomOnly = 0x00,
}

enum RomSizes {
    KiB32,
    KiB64,
    KiB128,
    KiB256,
    KiB512,
    MiB1,
    MiB2,
    MiB4,
    MiB8,
}

struct RomSizeData {
    value: u8,
    size: RomSizes,
}

const ROM_SIZE_00: RomSizeData = RomSizeData {
    value: 0x0,
    size: RomSizes::KiB32,
};

#[derive(Clone)]
enum RamSizes {
    None = 0x00,
    KiB8 = 0x02,
    KiB32 = 0x03,
    KiB128 = 0x04,
    KiB64 = 0x05,
}

struct Header {
    logo: &'static [u8],             // 0x0104 - 0x0133
    title: [char; 16],               // 0x0134 - 0x0143
    manufacturer: [char; 4],         // 0x013F - 0x0142
    cgb_flag: u8,                    // 0x0143
    publisher: [char; 2],            // 0x0144 - 0x0145
    sgb_flag: u8,                    // 0x0146
    cartridge_type: CartridgeMapper, // 0x0147
    rom_size: RomSizeData,           // 0x0148
    ram_size: RamSizes,              // 0x0149
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
            rom_size: ROM_SIZE_00,
            ram_size: RamSizes::None,
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

struct RomBank {
    bytes: [u8; 0x4000],
}

pub struct RomBuilder {
    // header
    header_data: Header,
    data: Vec<u8>, // bytes for the rom!
}

impl RomBuilder {
    pub fn new() -> RomBuilder {
        RomBuilder {
            header_data: Header::default(),
            data: vec![0xff; 0x8000],
        }
    }

    fn calculate_header_checksum(&mut self) {
        let mut checksum: u8 = 0;

        for addr in 0x0134..0x014d {
            checksum = u8::wrapping_sub(u8::wrapping_sub(checksum, self.data[addr]), 1);
        }

        self.header_data.checksum = checksum;
        self.data[0x014d] = self.header_data.checksum;
    }

    pub fn write_values(value: Value) -> Result<(), String> {
        // enforce cartridge type from header
        todo!()
    }

    fn char_to_vec(chars: &[char]) -> Vec<u8> {
        chars.into_iter().map(|c| *c as u8).collect()
    }

    fn write_header(&mut self) {
        // build placholder bytes 0x100 - 0x103?
        // nop; jp $0150

        self.data[0x0104..0x0134].clone_from_slice(LOGO_BYTES);

        self.data[0x0134..0x0144]
            .clone_from_slice(RomBuilder::char_to_vec(&self.header_data.title).as_slice());

        self.data[0x013f..0x0143]
            .clone_from_slice(RomBuilder::char_to_vec(&self.header_data.manufacturer).as_slice());

        self.data[0x0143] = self.header_data.cgb_flag;

        self.data[0x0144..0x0146]
            .clone_from_slice(RomBuilder::char_to_vec(&self.header_data.publisher).as_slice());

        self.data[0x0146] = self.header_data.sgb_flag;

        self.data[0x0147] = self.header_data.cartridge_type.clone() as u8;
        self.data[0x0148] = self.header_data.rom_size.value;
        self.data[0x0149] = self.header_data.ram_size.clone() as u8;
        self.data[0x014A] = self.header_data.destination_code as u8;
        self.data[0x014B] = self.header_data.old_publisher;
        self.data[0x014C] = self.header_data.mask_rom_number;
        self.data[0x014D] = self.header_data.checksum;
        self.data[0x014E..0x0150].clone_from_slice(&self.header_data.global_checksum);

        self.calculate_header_checksum();
    }

    pub fn build_rom(&mut self) -> Rom {
        // build placeholder bytes?
        self.write_header();

        Rom {
            data: self.data.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::rom_builder::{RomBuilder, LOGO_BYTES};

    #[test]
    /// The Game Boy boot ROM expects the 'logo bytes' to be found
    /// in the address range 0x104 - 0x133 in order to boot. This
    /// tests that the final ROM built contains the correct bytes
    /// for this check.
    fn check_logo_dump() {
        let mut builder = RomBuilder::new();
        let rom = builder.build_rom();

        assert_eq!(rom.data.as_slice()[0x0104..0x0134], *LOGO_BYTES);
    }
}
