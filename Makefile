cpu-test:
	cd nes_cpu_test; cargo build --release
	target/release/nes_cpu_test --quiet -f nes_cpu_test/src/6502_functional_test.bin
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_misc/rom_singles/01-abs_x_wrap.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/01-basics.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/02-implied.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/03-immediate.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/04-zero_page.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/05-zp_xy.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/06-absolute.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/07-abs_xy.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/08-ind_x.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/09-ind_y.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/10-branches.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/11-stack.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/12-jmp_jsr.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/13-rts.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/14-rti.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/15-brk.nes
	target/release/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/rom_singles/16-special.nes
#	target/release/nes_cpu_test --quiet -f ../nes-test-roms/cpu_dummy_reads/cpu_dummy_reads.nes

wasm-debug-build:
	cd nes_web; wasm-pack build --debug

web-start: wasm-debug-build
	cd www; NODE_OPTIONS=--openssl-legacy-provider npm start

