use nes_core::ines::{FileVersion, Header, INesFile};

#[derive(clap::Args)]
pub struct InfoAction {}

impl InfoAction {
    pub fn run(&self, f: &INesFile) -> Result<(), anyhow::Error> {
        print_key_values(&header_to_key_values(f.header(), f.file_version()));
        Ok(())
    }
}

fn header_to_key_values(h: &Header, version: FileVersion) -> Vec<(&str, String)> {
    let bool_to_string = |b: bool| if b { "yes" } else { "no" }.to_string();
    let arrangement_to_string = |a: &nes_core::ines::NametableArrangement| match a {
        nes_core::ines::NametableArrangement::Vertical => "vertical".to_string(),
        nes_core::ines::NametableArrangement::Horizontal => "horizontal".to_string(),
    };

    let mut values = vec![
        ("File version", format!("{:?}", version)),
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
            arrangement_to_string(&h.nametable_arrangement),
        ),
    ];

    if let Some(prg_ram_size) = h.prg_ram_size {
        values.push(("PRG RAM bytes", prg_ram_size.to_string()));
    }

    if version == FileVersion::Nes20 {
        values.push(("Submapper number", h.submapper_no.unwrap_or(0).to_string()));
        values.push(("PRG ROM bytes", h.prg_rom_size.to_string()));
        values.push(("CHR ROM bytes", h.chr_rom_size.to_string()));
        values.push(("Console type", h.console_type.to_string()));
        values.push(("PRG NVRAM bytes", h.prg_nvram_size.unwrap_or(0).to_string()));
        values.push(("CHR RAM bytes", h.chr_ram_size.unwrap_or(0).to_string()));
        values.push(("CHR NVRAM bytes", h.chr_nvram_size.unwrap_or(0).to_string()));
        values.push(("Timing mode", h.timing_mode.unwrap_or(0).to_string()));
        values.push(("Misc ROM count", h.misc_rom_count.unwrap_or(0).to_string()));
        values.push((
            "Expansion device",
            h.default_expansion_device.unwrap_or(0).to_string(),
        ));

        if let Some(vs_ppu_type) = h.vs_ppu_type {
            values.push(("Vs. PPU type", vs_ppu_type.to_string()));
        }
        if let Some(vs_hardware_type) = h.vs_hardware_type {
            values.push(("Vs. hardware type", vs_hardware_type.to_string()));
        }
        if let Some(extended_console_type) = h.extended_console_type {
            values.push(("Extended console", extended_console_type.to_string()));
        }
    }

    values
}

fn print_key_values(key_values: &[(&str, String)]) {
    let max_key_len = key_values.iter().map(|(k, _)| k.len()).max().unwrap();
    for (key, value) in key_values {
        println!("{:width$}: {}", key, value, width = max_key_len);
    }
}
