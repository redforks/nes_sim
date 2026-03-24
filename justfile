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
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/2-nmi_add_brk.nes

nes-test:
    cargo build -p nes_cpu_test -- -f ../nes-test-roms/other/nestest.nes --start-pc 0xC000

rom-test: cpu-test instr_misc instr_test-v5 instr_test-v3 nes-test cpu_interrupts_v2
    # cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/ppu_vbl_nmi.nes
    # cargo run -p nes_cpu_test --release -- -f ../nes-test-roms/cpu_dummy_reads/cpu_dummy_reads.nes

wasm-debug-build:
    cd nes_web && wasm-pack build --debug

web-start: wasm-debug-build
    cd www && NODE_OPTIONS=--openssl-legacy-provider npm start

test-cov:
    cd nes_core; cargo tarpaulin --skip-clean --out Html --engine llvm --output-dir /tmp/tarpaulin-output-dir
    xdg-open file:///tmp/tarpaulin-output-dir/tarpaulin-report.html

test-cov-text:
    cd nes_core; cargo tarpaulin --skip-clean --stderr --out Markdown --engine llvm --output-dir /tmp/tarpaulin-output-dir
