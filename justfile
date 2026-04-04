cpu-test:
    cargo run -p nes_cpu_test --release -- --quiet -f nes_cpu_test/src/6502_functional_test.bin

instr_misc:
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_misc/instr_misc.nes

instr_test-v3:
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v3/all_instrs.nes

instr_test-v5:
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/all_instrs.nes

cpu_interrupts_v2:
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/1-cli_latency.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/2-nmi_and_brk.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/3-nmi_and_irq.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/4-irq_and_dma.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/5-branch_delays_irq.nes

nestest:
    cargo run -p nes_cpu_test -- -f ../nes-test-roms/other/nestest.nes --start-pc 0xC000

rom-test: cpu-test instr_misc instr_test-v5 instr_test-v3 nestest cpu_interrupts_v2
    # cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/ppu_vbl_nmi.nes
    # cargo run -p nes_cpu_test --release -- -f ../nes-test-roms/cpu_dummy_reads/cpu_dummy_reads.nes

unit-test:
    cargo test

instr-timing:
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_timing/instr_timing.nes

ppu-vbl-nmi:
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/01-vbl_basics.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/02-vbl_set_time.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/03-vbl_clear_time.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/04-nmi_control.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/05-nmi_timing.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/06-suppression.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/07-nmi_on_timing.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/08-nmi_off_timing.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/09-even_odd_frames.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/rom_singles/10-even_odd_timing.nes

vbl-nmi-timing:
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/vbl_nmi_timing/1.frame_basics.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/vbl_nmi_timing/2.vbl_timing.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/vbl_nmi_timing/3.even_odd_frames.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/vbl_nmi_timing/4.vbl_clear_timing.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/vbl_nmi_timing/5.nmi_suppression.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/vbl_nmi_timing/6.nmi_disable.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/vbl_nmi_timing/7.nmi_timing.nes

passed: unit-test cpu-test instr_misc instr_test-v5 instr_test-v3 nestest instr-timing ppu-vbl-nmi vbl-nmi-timing
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/1-cli_latency.nes

wasm-debug-build:
    cd nes_web && wasm-pack build --release

web-start: wasm-debug-build
    cd www && NODE_OPTIONS=--openssl-legacy-provider pnpm run start

test-cov:
    cd nes_core; cargo tarpaulin --skip-clean --out Html --engine llvm --output-dir /tmp/tarpaulin-output-dir
    xdg-open file:///tmp/tarpaulin-output-dir/tarpaulin-report.html

test-cov-text:
    cd nes_core; cargo tarpaulin --skip-clean --stderr --out Markdown --engine llvm --output-dir /tmp/tarpaulin-output-dir
