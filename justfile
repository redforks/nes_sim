cpu-test:
    cargo run -p nes_cpu_test --release -- --quiet -f nes_cpu_test/src/6502_functional_test.bin
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_misc/rom_singles/01-abs_x_wrap.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/01-basics.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/02-implied.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/03-immediate.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/04-zero_page.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/05-zp_xy.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/06-absolute.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/07-abs_xy.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/08-ind_x.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/09-ind_y.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/10-branches.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/11-stack.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/12-jmp_jsr.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/13-rts.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/14-rti.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/15-brk.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/16-special.nes
    cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/1-cli_latency.nes
    # cargo run -p nes_cpu_test --release -- --quiet -f ../nes-test-roms/ppu_vbl_nmi/ppu_vbl_nmi.nes
    # cargo run -p nes_cpu_test --release -- -f ../nes-test-roms/cpu_dummy_reads/cpu_dummy_reads.nes

nes-test:
    cargo build -p nes_cpu_test
    timeout 45 target/debug/nes_cpu_test -f ../nes-test-roms/other/nestest.nes --start-pc 0xC000 --max-instructions 8991

wasm-debug-build:
    cd nes_web && wasm-pack build --debug

web-start: wasm-debug-build
    cd www && NODE_OPTIONS=--openssl-legacy-provider npm start

test-cov:
    cd nes_core; cargo tarpaulin --skip-clean --out Html --engine llvm --output-dir /tmp/tarpaulin-output-dir
    xdg-open file:///tmp/tarpaulin-output-dir/tarpaulin-report.html

test-cov-text:
    cd nes_core; cargo tarpaulin --skip-clean --stderr --out Markdown --engine llvm --output-dir /tmp/tarpaulin-output-dir
