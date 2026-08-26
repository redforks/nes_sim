use super::plugin::{
    CompositePlugin, Console, DetectDeadLoop, ExitTestPlugin, FramePngDump, ImageExit,
    MaxInstructions, MonitorTestStatus, NametableConsole, NesReportPlugin, PngFrameMatch,
    ReportNesTestResult, ReportPlugin, Timeout,
};
use super::zapper_test::ZapperAction;
use image::RgbaImage;
use nes_core::{
    Plugin, SystemClock, ines::INesFile, machine::Machine, mcu::RamMcu, nes_machine::NesMachine,
    render::ImageRender,
};
use std::{
    io::Read,
    path::{Path, PathBuf},
    time::Duration,
};

mod driver;

pub use driver::WavRecorder;

pub enum Image {
    Bin(Box<[u8; 64 * 1024]>),
    INes {
        nes_file: Box<INesFile>,
        file_name: PathBuf,
    },
}

impl Image {
    pub fn create_machine(
        &self,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        match self {
            Image::Bin(arr) => self.create_bin_machine(arr, quiet, start_pc, max_instructions),
            Image::INes {
                nes_file,
                file_name,
            } => self.create_ines_machine(nes_file, file_name, quiet, start_pc, max_instructions),
        }
    }

    fn create_bin_machine(
        &self,
        arr: &[u8; 64 * 1024],
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        let mcu = RamMcu::new(*arr);
        let mut plugins: Vec<Box<dyn Plugin<_>>> = vec![
            Box::new(ReportPlugin::create(quiet)),
            Box::new(ExitTestPlugin::new()),
            Box::<ImageExit>::default(),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = Machine::with_plugin(plugin, mcu);
        let mut clock = SystemClock::default();
        let mut drain_plugin = nes_core::EmptyPlugin::new();
        while !machine.cpu_mut().microcodes_empty() {
            machine.cpu_mut().tick(&mut drain_plugin, clock);
            clock = clock.inc();
        }
        match start_pc {
            Some(pc) => machine.set_pc(pc),
            None => machine.set_pc(0x400),
        }
        MachineWrapper::Bin(Box::new(machine), clock)
    }

    fn create_ines_machine(
        &self,
        ines: &INesFile,
        file_name: &Path,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        if let Some(f) = file_name.file_name().and_then(|f| f.to_str()) {
            if f == "scanline.nes" {
                return self.create_scanline_machine(ines, quiet, start_pc, max_instructions);
            } else if f == "mmc1_a12.nes" {
                return self.create_mmc1_a12_machine(ines, quiet, start_pc, max_instructions);
            } else if f == "demo_ntsc.nes"
                && file_name
                    .as_os_str()
                    .to_str()
                    .is_some_and(|s| s.contains("nmi_sync"))
            {
                return self.create_nmi_sync_machine(ines, quiet, start_pc, max_instructions);
            } else if let Some(stem) = f.strip_suffix(".nes")
                && stem.starts_with("vrctest")
            {
                return self.create_exp_png_machine(
                    ines,
                    quiet,
                    start_pc,
                    max_instructions,
                    vec![format!("{}.png", stem)],
                    Duration::from_secs(2),
                );
            } else if matches!(
                f,
                "zapper_flip.nes" | "zapper_light.nes" | "zapper_stream.nes" | "zapper_trigger.nes"
            ) {
                // Tetanes Zapper light-gun ROMs: no self-reporting protocol;
                // the harness (zapper_test.rs) scripts input per frame and
                // verifies blessed frames + $4011 click counts. The machine
                // itself only needs a rendered framebuffer.
                return self.create_zapper_machine(ines, quiet, start_pc, max_instructions);
            } else if file_name
                .to_str()
                .is_some_and(|p| p.contains("test-roms/mapper/"))
                && f == "big_chr_ram.nes"
            {
                // Tetanes mapper ROM (Damian Yerrick's "big CHR RAM test"):
                // draws through CHR-RAM windows, then waits for Start. Success
                // is a blessed snapshot of the rendered frames under
                // png-exps/ (frame-snapshot scheme ported from tetanes-core's
                // tests.json: frames 10 and 80; the Start press at frame 11
                // is supplied by --press start@11 in the recipe).
                return self.create_exp_png_machine(
                    ines,
                    quiet,
                    start_pc,
                    max_instructions,
                    vec![
                        "big_chr_ram-f10.png".to_string(),
                        "big_chr_ram-f80.png".to_string(),
                    ],
                    // 80 frames of emulation need headroom over the other
                    // visual tests (debug builds run ~10x slower), but must
                    // stay under the recipe's outer `timeout 15` so the
                    // plugin's diagnostic fires first.
                    Duration::from_secs(10),
                );
            } else if file_name
                .to_str()
                .is_some_and(|p| p.contains("test-roms/ppu/"))
                && matches!(
                    f,
                    "color.nes" | "ntsc_torture.nes" | "palette.nes" | "_240pee.nes"
                )
            {
                // Visual-only tetanes ROMs: no self-reporting protocol, so success
                // is a blessed snapshot of the rendered frame under png-exps/
                // (frame-snapshot scheme ported from tetanes-core's tests.json;
                // frame numbers quoted from there).
                return self.create_exp_png_machine(
                    ines,
                    quiet,
                    start_pc,
                    max_instructions,
                    vec![format!(
                        "{}.png",
                        f.strip_suffix(".nes").expect("checked suffix")
                    )],
                    Duration::from_secs(2),
                );
            } else if file_name
                .to_str()
                .is_some_and(|p| p.contains("blargg_ppu_tests_2005.09.15b"))
            {
                return self.create_blargg_result_code_machine(
                    ines,
                    quiet,
                    start_pc,
                    max_instructions,
                    Duration::from_secs(5),
                );
            } else if file_name
                .to_str()
                .is_some_and(|p| p.contains("blargg_apu_2005.07.30"))
            {
                return self.create_blargg_result_code_machine(
                    ines,
                    quiet,
                    start_pc,
                    max_instructions,
                    Duration::from_secs(2),
                );
            } else if file_name
                .to_str()
                .is_some_and(|p| p.contains("blargg_nes_cpu_test5"))
            {
                return self.create_blargg_cpu_test5_machine(
                    ines,
                    quiet,
                    start_pc,
                    max_instructions,
                );
            }
        }

        // Build the composite plugin step by step to handle type coercion
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<(), ()>>>> = vec![
            Box::new(NesReportPlugin::create(quiet)),
            Box::new(DetectDeadLoop::<1>::new()),
            Box::new(DetectDeadLoop::<2>::new()),
        ];
        if file_name.file_name().is_some_and(|f| f == "nestest.nes") {
            plugins.push(Box::new(ReportNesTestResult::new()));
        } else if let Some(file_name_str) = file_name.file_name().and_then(|f| f.to_str()) {
            if matches!(
                file_name_str,
                "bntest_h.nes" | "bntest_v.nes" | "bntest_aorom.nes"
            ) {
                plugins.push(Box::new(
                    NametableConsole::with_tall_text_magic_success_word("0123456789ABCDEF"),
                ));
                plugins.push(Box::new(Timeout::new(Duration::from_secs(5))));
            } else if file_name.to_str().is_some_and(|p| {
                p.contains("vbl_nmi_timing")
                    || p.contains("branch_timing_tests")
                    || p.contains("cpu_dummy_reads")
                    || p.contains("cpu_timing_test6")
                    || p.contains("sprite_hit_tests_2005.10.05")
                    || p.contains("sprite_overflow_tests")
                    || p.contains("dmc_dma_during_read4")
                    || p.contains("mmc3_irq_tests")
            }) {
                // These two blargg ROMs never print "Passed": they print a CRC-32
                // of their output and then dead-loop, so the harness detects
                // success by matching that CRC in the console text (the Timeout
                // below catches mismatches and hangs).
                //
                // Both magic words are quoted from the expected-output comments
                // in the test ROM sources
                // (../nes-test-roms/dmc_dma_during_read4/source/):
                // - double_2007_read.s lists "85CFD627 or F018C287 or 440EF923
                //   or E52F41A5". The outcome depends on CPU-PPU synchronization
                //   at reset; our deterministic alignment produces the
                //   first-listed variant (row "22 44 55 66 77" -> 85CFD627).
                // - dma_2007_read.s lists "159A7A8F or 5E3DF9C4". Our alignment
                //   produces the second-listed variant (row "44 55" -> 5E3DF9C4).
                //
                // The previously blessed words ("D84F6815", "159A7A8F") were
                // artifacts of an emulator that serviced the back-to-back $2007
                // reads fully and missed the DMC-DMA collision window; neither
                // appears in the ROM sources' accepted lists.
                if file_name
                    .file_name()
                    .is_some_and(|f| f == "double_2007_read.nes")
                {
                    plugins.push(Box::new(NametableConsole::with_magic_success_word(
                        "85CFD627",
                    )));
                    plugins.push(Box::new(Timeout::new(Duration::from_secs(5))));
                } else if file_name
                    .file_name()
                    .is_some_and(|f| f == "dma_2007_read.nes")
                {
                    plugins.push(Box::new(NametableConsole::with_magic_success_word(
                        "5E3DF9C4",
                    )));
                    plugins.push(Box::new(Timeout::new(Duration::from_secs(5))));
                } else {
                    plugins.push(Box::new(NametableConsole::default()));
                }
            } else if file_name
                .to_str()
                .is_some_and(|p| p.contains("test-roms/apu/"))
                && matches!(
                    file_name_str,
                    "test_1.nes"
                        | "test_2.nes"
                        | "test_3.nes"
                        | "test_4.nes"
                        | "test_5.nes"
                        | "test_6.nes"
                        | "test_7.nes"
                        | "test_8.nes"
                        | "test_9.nes"
                        | "test_10.nes"
                )
            {
                // blargg's forum APU tests (frame counter timing probes): they
                // print "TEST PASSED"/"TEST FAILED" as ASCII tiles at a nametable
                // offset (hence read_plain_console's leading-NUL skip) and then
                // hang in a CLV/BVC spin — no $6000 signature, so the nametable
                // text is the verdict (PassedOrFailed) and the Timeout catches a
                // hang with neither verdict.
                plugins.push(Box::new(NametableConsole::default()));
                plugins.push(Box::new(Timeout::new(Duration::from_secs(5))));
            } else {
                plugins.push(Box::<Console>::default());
                plugins.push(Box::<MonitorTestStatus>::default());
            }
        } else {
            plugins.push(Box::<Console>::default());
            plugins.push(Box::<MonitorTestStatus>::default());
        }
        if max_instructions > 0 {
            // MaxInstructions is generic over Mcu; need to coerce type. MaxInstructions doesn't use Mcu methods so this is fine.
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(ines, plugin, (), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        if file_name
            .to_str()
            .is_some_and(|p| p.contains("cpu_timing_test6"))
        {
            // test OFFICIAL + UNDOCUMENTED instructions
            machine.press_controller_a(nes_core::nes::controller::Button::B);
        }
        MachineWrapper::INes(Box::new(machine))
    }
    /// blargg's 2005-era test ROM families that report a numeric result code
    /// as ASCII nametable text ("$01" = all tests passed, per each set's
    /// readme/tests notes) instead of any $6000 status protocol: the NTSC PPU
    /// set (2005.09.15b, timeout 5 s) and the APU length/frame-counter set
    /// (2005.07.30, timeout 2 s). Each ROM prints its code and then
    /// dead-loops in a bare `jmp self`, which DetectDeadLoop would report as
    /// a spurious exit-0 pass on any ROM whose final code differs from the
    /// magic word (e.g. power_up_palette did render "$02" before nes_core
    /// adopted blargg's power-up palette). So this family omits
    /// DetectDeadLoop: the NametableConsole magic word is the only success
    /// verdict, and the Timeout turns any other final code — or a hang
    /// before printing — into a nonzero exit.
    fn create_blargg_result_code_machine(
        &self,
        ines: &INesFile,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
        timeout: Duration,
    ) -> MachineWrapper {
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<(), ()>>>> = vec![
            Box::new(NesReportPlugin::create(quiet)),
            Box::new(NametableConsole::with_magic_success_word("$01")),
            Box::new(Timeout::new(timeout)),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(ines, plugin, (), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::INes(Box::new(machine))
    }

    /// blargg's NES CPU test set v5 (cpu.nes: all instructions incl.
    /// undocumented; official.nes: official only). The ROMs stream progress as
    /// nametable text ("Running tests...", one line per sub-test) and finish
    /// with "All tests complete" — those strings ship in the ROM binaries even
    /// though the set's bundled source/ predates them. The verdict is the
    /// guarded magic word: any failure marker ("Failed", "Error ",
    /// "Errors: <nonzero>") stops immediately with a failure so a completion
    /// epilogue can't mask errors, while a clean screen containing the word
    /// exits 0. As with the 2005 PPU family this omits DetectDeadLoop: the
    /// ROMs terminate in a clean infinite loop that dead-loop detection would
    /// report as a vacuous exit-0 pass regardless of the printed verdict.
    ///
    /// Two ROM-specific sampling quirks, both verified against the real ROMs:
    /// the shell zeroes PPUCTRL as it prints the verdict, so the console
    /// samples without the rendering-enabled gate
    /// (`sampling_without_rendering`); and the console scrolls its text up as
    /// sub-tests complete, so the fixed 960-tile window at 0x2000 goes stale
    /// mid-run and the verdict must be scanned across the whole nametable
    /// address space (`with_full_nametable_scan`). The machines run on
    /// ImageRender like the scanline machine, where this family's end-to-end
    /// completion is verified. Timeout turns a hang (word never printed) into
    /// a structured failure; the justfile wraps the same 15 s budget with an
    /// outer `timeout 15`.
    fn create_blargg_cpu_test5_machine(
        &self,
        ines: &INesFile,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<ImageRender, ()>>>> = vec![
            Box::new(NesReportPlugin::create(quiet)),
            Box::new(
                NametableConsole::with_magic_success_word_unless_failed("All tests complete")
                    .sampling_without_rendering()
                    .with_full_nametable_scan(),
            ),
            Box::new(Timeout::new(Duration::from_secs(15))),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(ines, plugin, ImageRender::default_dimension(), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::Rendered(Box::new(machine))
    }

    fn create_exp_png_machine(
        &self,
        ines: &INesFile,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
        exp_img_paths: Vec<String>,
        timeout: Duration,
    ) -> MachineWrapper {
        let expected_pngs: Vec<PathBuf> = exp_img_paths
            .into_iter()
            .map(|f| {
                PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("src/png-exps/")
                    .join(f)
            })
            .collect();
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<ImageRender, ()>>>> = vec![
            Box::new(NesReportPlugin::create(quiet)),
            Box::new(PngFrameMatch::new(expected_pngs).expect("failed to load expected PNG")),
            Box::new(Timeout::new(timeout)),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(ines, plugin, ImageRender::default_dimension(), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::Rendered(Box::new(machine))
    }

    /// Zapper ROM machine: rendered framebuffer for light sensing, no
    /// frame-matching plugin — verdicts come from the scripted
    /// [`super::zapper_test::ZapperTest`] expectations in main's run loop.
    fn create_zapper_machine(
        &self,
        ines: &INesFile,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<ImageRender, ()>>>> = vec![
            Box::new(NesReportPlugin::create(quiet)),
            Box::new(Timeout::new(Duration::from_secs(5))),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(ines, plugin, ImageRender::default_dimension(), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::Rendered(Box::new(machine))
    }

    /// Blessing machine: renders until [`FramePngDump`] saves frame
    /// `target_frame` to `out_path`, then stops.
    pub fn create_dump_machine(
        &self,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
        target_frame: usize,
        out_path: PathBuf,
    ) -> MachineWrapper {
        let Image::INes { nes_file, .. } = self else {
            panic!("--dump-frame requires an iNES ROM (needs the PPU renderer)");
        };
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<ImageRender, ()>>>> = vec![
            Box::new(NesReportPlugin::create(quiet)),
            Box::new(FramePngDump::new(target_frame, out_path)),
            Box::new(Timeout::new(Duration::from_secs(15))),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(nes_file, plugin, ImageRender::default_dimension(), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::Rendered(Box::new(machine))
    }

    /// Audio-capture machine: runs the ROM with a [`WavRecorder`] as the APU
    /// sink and no test-protocol plugins; termination comes from the CLI's
    /// frame budget (`--frames`), after which the CLI exports the buffered
    /// samples as WAV.
    pub fn create_audio_dump_machine(
        &self,
        recorder: WavRecorder,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        let Image::INes { nes_file, .. } = self else {
            panic!("--dump-audio requires an iNES ROM (needs the NES APU)");
        };
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<(), WavRecorder>>>> =
            vec![Box::new(NesReportPlugin::create(quiet))];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(nes_file, plugin, (), recorder);
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::AudioDump(Box::new(machine))
    }

    fn create_mmc1_a12_machine(
        &self,
        ines: &INesFile,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        self.create_exp_png_machine(
            ines,
            quiet,
            start_pc,
            max_instructions,
            vec!["mmc1_a12-exp.png".to_string()],
            Duration::from_secs(2),
        )
    }

    fn create_nmi_sync_machine(
        &self,
        ines: &INesFile,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        self.create_exp_png_machine(
            ines,
            quiet,
            start_pc,
            max_instructions,
            vec![
                "nmi-sync-ntsc-exp-1.png".to_string(),
                "nmi-sync-ntsc-exp-2.png".to_string(),
            ],
            Duration::from_secs(2),
        )
    }

    fn create_scanline_machine(
        &self,
        ines: &INesFile,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        let expected_png = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/scanline-exp.png");
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<ImageRender, ()>>>> = vec![
            Box::new(NesReportPlugin::create(quiet)),
            Box::new(NametableConsole::default()),
            Box::new(
                PngFrameMatch::new(vec![expected_png]).expect("failed to load scanline-exp.png"),
            ),
            Box::new(Timeout::new(Duration::from_secs(2))),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(ines, plugin, ImageRender::default_dimension(), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::Rendered(Box::new(machine))
    }
}

// Type aliases to avoid >> parsing issues in enums
mod machine_types {
    use super::*;
    use nes_core::nes_machine::NesMachine;

    pub type BinMcu = RamMcu<{ 64 * 1024 }>;
    pub type BinPlugin = CompositePlugin<BinMcu>;
    pub type BinMachine = Machine<BinPlugin, BinMcu>;

    pub type INesPlugin = CompositePlugin<nes_core::nes::NesMcu<(), ()>>;
    pub type INesMachine = NesMachine<INesPlugin, (), ()>;

    pub type ImageRenderPlugin = CompositePlugin<nes_core::nes::NesMcu<ImageRender, ()>>;
    pub type RenderedMachine = NesMachine<ImageRenderPlugin, ImageRender, ()>;
    pub type AudioDumpPlugin = CompositePlugin<nes_core::nes::NesMcu<(), WavRecorder>>;
    pub type AudioDumpMachine = NesMachine<AudioDumpPlugin, (), WavRecorder>;
}

pub enum MachineWrapper {
    Bin(Box<machine_types::BinMachine>, SystemClock),
    INes(Box<machine_types::INesMachine>),
    Rendered(Box<machine_types::RenderedMachine>),
    AudioDump(Box<machine_types::AudioDumpMachine>),
}

impl MachineWrapper {
    pub fn tick(&mut self) -> nes_core::ExecuteResult {
        match self {
            MachineWrapper::Bin(m, clock) => {
                let r = m.tick(*clock);
                *clock = clock.inc();
                r
            }
            MachineWrapper::INes(m) => m.tick(),
            MachineWrapper::Rendered(m) => m.tick(),
            MachineWrapper::AudioDump(m) => m.tick(),
        }
    }

    pub fn reset(&mut self) {
        match self {
            MachineWrapper::Bin(m, _) => m.reset(),
            MachineWrapper::INes(m) => m.reset(),
            MachineWrapper::Rendered(m) => m.reset(),
            MachineWrapper::AudioDump(m) => m.reset(),
        }
    }

    /// True while the PPU is in vblank (vblank flag set).
    pub fn in_vblank(&self) -> bool {
        match self {
            MachineWrapper::Bin(..) => panic!("in_vblank requires an iNES ROM (needs the PPU)"),
            MachineWrapper::INes(m) => m.mcu().ppu().in_vblank(),
            MachineWrapper::Rendered(m) => m.mcu().ppu().in_vblank(),
            MachineWrapper::AudioDump(m) => m.mcu().ppu().in_vblank(),
        }
    }

    /// Total DMC $4011 DAC-register writes so far (Zapper ROM click counter).
    pub fn dmc_dac_writes(&self) -> u64 {
        match self {
            MachineWrapper::Bin(..) => panic!("dmc_dac_writes requires an iNES ROM"),
            MachineWrapper::INes(m) => m.mcu().apu().dmc_dac_writes(),
            MachineWrapper::Rendered(m) => m.mcu().apu().dmc_dac_writes(),
            MachineWrapper::AudioDump(m) => m.mcu().apu().dmc_dac_writes(),
        }
    }

    /// The rendered framebuffer, for frame-snapshot comparison.
    pub fn renderer_image(&self) -> &RgbaImage {
        match self {
            MachineWrapper::Rendered(m) => m.mcu().ppu().renderer().borrow_image(),
            _ => panic!("renderer_image requires a rendered iNES machine"),
        }
    }

    /// Apply one scripted Zapper event.
    pub fn apply_zapper_action(&mut self, action: ZapperAction) {
        match self {
            MachineWrapper::Bin(..) => panic!("zapper actions require an iNES ROM"),
            MachineWrapper::INes(m) => match action {
                ZapperAction::Connect => m.connect_zapper(true),
                ZapperAction::Aim(x, y) => m.aim_zapper(x, y),
                ZapperAction::Trigger => m.trigger_zapper(),
            },
            MachineWrapper::Rendered(m) => match action {
                ZapperAction::Connect => m.connect_zapper(true),
                ZapperAction::Aim(x, y) => m.aim_zapper(x, y),
                ZapperAction::Trigger => m.trigger_zapper(),
            },
            MachineWrapper::AudioDump(m) => match action {
                ZapperAction::Connect => m.connect_zapper(true),
                ZapperAction::Aim(x, y) => m.aim_zapper(x, y),
                ZapperAction::Trigger => m.trigger_zapper(),
            },
        }
    }
    pub fn frame_no(&self) -> usize {
        match self {
            MachineWrapper::Bin(..) => {
                panic!("--press requires an iNES ROM (needs the PPU frame counter)")
            }
            MachineWrapper::INes(m) => m.frame_no(),
            MachineWrapper::Rendered(m) => m.frame_no(),
            MachineWrapper::AudioDump(m) => m.frame_no(),
        }
    }

    pub fn press_controller_a(&mut self, button: nes_core::nes::controller::Button) {
        match self {
            MachineWrapper::Bin(..) => panic!("--press requires an iNES ROM"),
            MachineWrapper::INes(m) => m.press_controller_a(button),
            MachineWrapper::Rendered(m) => m.press_controller_a(button),
            MachineWrapper::AudioDump(m) => m.press_controller_a(button),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("IO error")]
    IOError(#[from] std::io::Error),
    #[error("invalid iNES file format")]
    InvalidINes(#[from] nes_core::ines::FormatError),
}

fn read_file_bytes(f: &Path) -> Result<Vec<u8>, LoadError> {
    let mut f = std::fs::File::open(f)?;
    let mut buf = Vec::new();
    f.read_to_end(&mut buf).unwrap();
    Ok(buf)
}

pub fn load_image(f: PathBuf) -> Result<Image, LoadError> {
    // Resolve relative paths against workspace directory
    let path = if f.is_relative() {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("workspace parent directory not found")
            .join(&f)
    } else {
        f
    };

    if path.extension().is_some_and(|ext| ext == "bin") {
        load_bin(&path)
    } else if is_nes_file(&path) {
        load_rom(path)
    } else {
        panic!("unknown file type");
    }
}

fn load_bin(f: &Path) -> Result<Image, LoadError> {
    let buf = read_file_bytes(f)?;
    assert_eq!(buf.len(), 64 * 1024);
    let arr: [u8; 64 * 1024] = buf.try_into().expect("image file length is not 64k");
    Ok(Image::Bin(Box::new(arr)))
}

fn is_nes_file(f: &Path) -> bool {
    let buf = read_file_bytes(f).unwrap();
    INesFile::is_valid(&buf)
}

fn load_rom(f: PathBuf) -> Result<Image, LoadError> {
    Ok(Image::INes {
        nes_file: Box::new(INesFile::new(read_file_bytes(&f)?)?),
        file_name: f,
    })
}
