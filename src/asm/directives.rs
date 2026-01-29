use strum_macros::{AsRefStr, IntoStaticStr};

#[derive(IntoStaticStr, AsRefStr)]
pub enum Directives {
    #[strum(serialize = "section")]
    Section,
    #[strum(serialize = "entry")]
    Entry,
    #[strum(serialize = "data")]
    Data,
    #[strum(serialize = "global")]
    GlobalLabel,
}

#[derive(Debug, Clone)]
pub enum Section {
    EntrySection,
    RomBank0,
    RomBank1,
}

impl Section {
    pub fn get_section_index(&self) -> usize {
        match self {
            Section::EntrySection => 0,
            Section::RomBank0 => 0,
            Section::RomBank1 => 1,
        }
    }

    pub fn get_section_size(&self) -> usize {
        match self {
            // There are only 4 bytes available before the header.
            Section::EntrySection => 4,
            // The header runs up until 0x150 in ROM Bank 0.
            Section::RomBank0 => 0x4000 - 0x150,
            Section::RomBank1 => 0x4000,
        }
    }
}

pub fn match_directive(directive: &str) -> Option<Directives> {
    match &directive[1..] {
        val if val == Directives::Section.as_ref() => Some(Directives::Section),
        val if val == Directives::Data.as_ref() => Some(Directives::Data),
        val if val == Directives::Entry.as_ref() => Some(Directives::Entry),
        val if val == Directives::GlobalLabel.as_ref() => Some(Directives::GlobalLabel),
        _ => None,
    }
}
