pub enum Directives {
    Section,
    Entry,
    Data,
}

#[derive(Debug, Clone)]
pub enum Section {
    EntrySection,
    RomBank0,
    RomBank1,
}

pub fn match_directive(directive: &str) -> Option<Directives> {
    match &directive[1..] {
        "section" => Some(Directives::Section),
        "data" => Some(Directives::Data),
        "entry" => Some(Directives::Entry),
        _ => None,
    }
}
