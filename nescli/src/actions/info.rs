use nes_core::ines::{Header, INesFile};

#[derive(clap::Args)]
pub struct InfoAction {}

impl InfoAction {
    pub fn run(&self, f: &INesFile) -> Result<(), anyhow::Error> {
        print_key_values(&header_to_key_values(f.header()));
        Ok(())
    }
}

fn header_to_key_values(h: &Header) -> [(&str, String); 7] {
    let bool_to_string = |b: bool| if b { "yes" } else { "no" }.to_string();

    [
        ("Mapper number", h.mapper_no.to_string()),
        ("PRG ROM", format!("{} x 16k", h.n_prg_pages)),
        ("CHR ROM", format!("{} x 8k", h.n_chr_pages)),
        (
            "Ignore mirror control",
            bool_to_string(h.ignore_mirror_control),
        ),
        ("Has trainer", bool_to_string(h.has_trainer)),
        ("Battery backed RAM", bool_to_string(h.battery_backed_ram)),
        (
            "Vertical or horizontal",
            bool_to_string(h.ver_or_hor_arrangement),
        ),
    ]
}

fn print_key_values(key_values: &[(&str, String)]) {
    let max_key_len = key_values.iter().map(|(k, _)| k.len()).max().unwrap();
    for (key, value) in key_values {
        println!("{:width$}: {}", key, value, width = max_key_len);
    }
}
