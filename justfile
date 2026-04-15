cpu-test: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f nes_cpu_test/src/6502_functional_test.bin

instr_misc: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/instr_misc/instr_misc.nes

instr_test-v3: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v3/all_instrs.nes

instr_test-v5: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/instr_test-v5/all_instrs.nes

cpu_interrupts_v2: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/1-cli_latency.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/2-nmi_and_brk.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/3-nmi_and_irq.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/4-irq_and_dma.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_interrupts_v2/rom_singles/5-branch_delays_irq.nes

branch_timing_tests: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/branch_timing_tests/1.Branch_Basics.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/branch_timing_tests/2.Backward_Branch.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/branch_timing_tests/3.Forward_Branch.nes

nestest:
    cargo run -p nes_cpu_test -- -f ../nes-test-roms/other/nestest.nes --start-pc 0xC000

unit-test:
    cargo test

instr_timing: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/instr_timing/instr_timing.nes

ppu_vbl_nmi: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/ppu_vbl_nmi/ppu_vbl_nmi.nes

build_nes_cpu_test:
    cargo build -p nes_cpu_test

vbl_nmi_timing: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/vbl_nmi_timing/1.frame_basics.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/vbl_nmi_timing/2.vbl_timing.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/vbl_nmi_timing/3.even_odd_frames.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/vbl_nmi_timing/4.vbl_clear_timing.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/vbl_nmi_timing/5.nmi_suppression.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/vbl_nmi_timing/6.nmi_disable.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/vbl_nmi_timing/7.nmi_timing.nes

cpu_dummy_reads: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_dummy_reads/cpu_dummy_reads.nes

cpu_dummy_writes: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_dummy_writes/cpu_dummy_writes_oam.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_dummy_writes/cpu_dummy_writes_ppumem.nes

cpu_exec_space: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_exec_space/test_cpu_exec_space_apu.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_exec_space/test_cpu_exec_space_ppuio.nes

cpu_reset: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_reset/ram_after_reset.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_reset/registers.nes

cpu_timing_test6: build_nes_cpu_test
    timeout 30 target/debug/nes_cpu_test --quiet -f ../nes-test-roms/cpu_timing_test6/cpu_timing_test.nes

oam_read: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/oam_read/oam_read.nes

oam_stress: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/oam_stress/oam_stress.nes

ppu_open_bus: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/ppu_open_bus/ppu_open_bus.nes

ppu_read_buffer: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/ppu_read_buffer/test_ppu_read_buffer.nes

sprite_hit_tests: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/01.basics.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/02.alignment.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/03.corners.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/04.flip.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/05.left_clip.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/06.right_edge.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/07.screen_bottom.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/08.double_height.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/09.timing_basics.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/10.timing_order.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/11.edge_timing.nes

sprite_overflow_tests: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_overflow_tests/1.Basics.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_overflow_tests/2.Details.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_overflow_tests/3.Timing.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_overflow_tests/4.Obscure.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprite_overflow_tests/5.Emulator.nes

dmc_dma_during_read4: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/dmc_dma_during_read4/dma_4016_read.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/dmc_dma_during_read4/dma_2007_read.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/dmc_dma_during_read4/dma_2007_write.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/dmc_dma_during_read4/read_write_2007.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/dmc_dma_during_read4/double_2007_read.nes

sprdma_and_dmc_dma: build_nes_cpu_test
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprdma_and_dmc_dma/sprdma_and_dmc_dma.nes
    target/debug/nes_cpu_test --quiet -f ../nes-test-roms/sprdma_and_dmc_dma/sprdma_and_dmc_dma_512.nes

[parallel]
passed: unit-test cpu-test instr_misc instr_test-v5 instr_test-v3 nestest instr_timing ppu_vbl_nmi vbl_nmi_timing cpu_interrupts_v2 branch_timing_tests cpu_dummy_reads cpu_dummy_writes cpu_exec_space cpu_reset cpu_timing_test6 oam_read oam_stress ppu_open_bus ppu_read_buffer sprite_hit_tests sprite_overflow_tests dmc_dma_during_read4

wasm-debug-build:
    cd nes_web && wasm-pack build --release

web-start: wasm-debug-build
    cd www && NODE_OPTIONS=--openssl-legacy-provider pnpm run start

test-cov:
    cd nes_core; cargo tarpaulin --skip-clean --out Html --engine llvm --output-dir /tmp/tarpaulin-output-dir
    xdg-open file:///tmp/tarpaulin-output-dir/tarpaulin-report.html

test-cov-text:
    cd nes_core; cargo tarpaulin --skip-clean --stderr --out Markdown --engine llvm --output-dir /tmp/tarpaulin-output-dir
