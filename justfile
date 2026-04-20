nes_cpu_test := "/tmp/nes-sim-target/debug/nes_cpu_test"

cpu-test: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f nes_cpu_test/src/6502_functional_test.bin

instr_misc: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_misc/instr_misc.nes

instr_test-v3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_test-v3/all_instrs.nes

instr_test-v5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_test-v5/all_instrs.nes

cpu_interrupts_v2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_interrupts_v2/cpu_interrupts.nes

branch_timing_tests_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/branch_timing_tests/1.Branch_Basics.nes

branch_timing_tests_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/branch_timing_tests/2.Backward_Branch.nes

branch_timing_tests_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/branch_timing_tests/3.Forward_Branch.nes

[parallel]
branch_timing_tests: branch_timing_tests_1 branch_timing_tests_2 branch_timing_tests_3

nestest:
    cargo run -p nes_cpu_test -- -f ../nes-test-roms/other/nestest.nes --start-pc 0xC000

unit-test:
    cargo test

instr_timing: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_timing/instr_timing.nes

ppu_vbl_nmi: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/ppu_vbl_nmi/ppu_vbl_nmi.nes

build_nes_cpu_test:
    cargo build -p nes_cpu_test

vbl_nmi_timing_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/vbl_nmi_timing/1.frame_basics.nes

vbl_nmi_timing_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/vbl_nmi_timing/2.vbl_timing.nes

vbl_nmi_timing_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/vbl_nmi_timing/3.even_odd_frames.nes

vbl_nmi_timing_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/vbl_nmi_timing/4.vbl_clear_timing.nes

vbl_nmi_timing_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/vbl_nmi_timing/5.nmi_suppression.nes

vbl_nmi_timing_6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/vbl_nmi_timing/6.nmi_disable.nes

vbl_nmi_timing_7: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/vbl_nmi_timing/7.nmi_timing.nes

[parallel]
vbl_nmi_timing: vbl_nmi_timing_1 vbl_nmi_timing_2 vbl_nmi_timing_3 vbl_nmi_timing_4 vbl_nmi_timing_5 vbl_nmi_timing_6 vbl_nmi_timing_7

cpu_dummy_reads: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_dummy_reads/cpu_dummy_reads.nes

cpu_dummy_writes_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_dummy_writes/cpu_dummy_writes_oam.nes

cpu_dummy_writes_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_dummy_writes/cpu_dummy_writes_ppumem.nes

[parallel]
cpu_dummy_writes: cpu_dummy_writes_1 cpu_dummy_writes_2

cpu_exec_space_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_exec_space/test_cpu_exec_space_apu.nes

cpu_exec_space_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_exec_space/test_cpu_exec_space_ppuio.nes

[parallel]
cpu_exec_space: cpu_exec_space_1 cpu_exec_space_2

cpu_reset_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_reset/ram_after_reset.nes

cpu_reset_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_reset/registers.nes

[parallel]
cpu_reset: cpu_reset_1 cpu_reset_2

cpu_timing_test6: build_nes_cpu_test
    timeout 30 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_timing_test6/cpu_timing_test.nes

oam_read: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/oam_read/oam_read.nes

oam_stress: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/oam_stress/oam_stress.nes

ppu_open_bus: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/ppu_open_bus/ppu_open_bus.nes

ppu_read_buffer: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/ppu_read_buffer/test_ppu_read_buffer.nes

sprite_hit_tests_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/01.basics.nes

sprite_hit_tests_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/02.alignment.nes

sprite_hit_tests_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/03.corners.nes

sprite_hit_tests_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/04.flip.nes

sprite_hit_tests_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/05.left_clip.nes

sprite_hit_tests_6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/06.right_edge.nes

sprite_hit_tests_7: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/07.screen_bottom.nes

sprite_hit_tests_8: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/08.double_height.nes

sprite_hit_tests_9: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/09.timing_basics.nes

sprite_hit_tests_10: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/10.timing_order.nes

sprite_hit_tests_11: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_hit_tests_2005.10.05/11.edge_timing.nes

[parallel]
sprite_hit_tests: sprite_hit_tests_1 sprite_hit_tests_2 sprite_hit_tests_3 sprite_hit_tests_4 sprite_hit_tests_5 sprite_hit_tests_6 sprite_hit_tests_7 sprite_hit_tests_8 sprite_hit_tests_9 sprite_hit_tests_10 sprite_hit_tests_11

sprite_overflow_tests_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_overflow_tests/1.Basics.nes

sprite_overflow_tests_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_overflow_tests/2.Details.nes

sprite_overflow_tests_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_overflow_tests/3.Timing.nes

sprite_overflow_tests_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_overflow_tests/4.Obscure.nes

sprite_overflow_tests_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprite_overflow_tests/5.Emulator.nes

[parallel]
sprite_overflow_tests: sprite_overflow_tests_1 sprite_overflow_tests_2 sprite_overflow_tests_3 sprite_overflow_tests_4 sprite_overflow_tests_5

dmc_dma_during_read4_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/dmc_dma_during_read4/dma_4016_read.nes

dmc_dma_during_read4_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/dmc_dma_during_read4/dma_2007_read.nes

dmc_dma_during_read4_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/dmc_dma_during_read4/dma_2007_write.nes

dmc_dma_during_read4_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/dmc_dma_during_read4/read_write_2007.nes

dmc_dma_during_read4_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/dmc_dma_during_read4/double_2007_read.nes

dmc_dma_during_read4: dmc_dma_during_read4_1 dmc_dma_during_read4_2 dmc_dma_during_read4_3 dmc_dma_during_read4_4 dmc_dma_during_read4_5

sprdma_and_dmc_dma_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprdma_and_dmc_dma/sprdma_and_dmc_dma.nes

sprdma_and_dmc_dma_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/sprdma_and_dmc_dma/sprdma_and_dmc_dma_512.nes

sprdma_and_dmc_dma: sprdma_and_dmc_dma_1 sprdma_and_dmc_dma_2

apu_mixer_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_mixer/dmc.nes

apu_mixer_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_mixer/noise.nes

apu_mixer_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_mixer/square.nes

apu_mixer_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_mixer/triangle.nes

[parallel]
apu_mixer: apu_mixer_1 apu_mixer_2 apu_mixer_3 apu_mixer_4

apu_reset_1: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_reset/4015_cleared.nes

apu_reset_2: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_reset/4017_timing.nes

apu_reset_3: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_reset/4017_written.nes

apu_reset_4: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_reset/irq_flag_cleared.nes

apu_reset_5: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_reset/len_ctrs_enabled.nes

apu_reset_6: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_reset/works_immediately.nes

[parallel]
apu_reset: apu_reset_1 apu_reset_2 apu_reset_3 apu_reset_4 apu_reset_5 apu_reset_6

apu_test: build_nes_cpu_test
    timeout 10 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/apu_test/apu_test.nes

scanline: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/scanline/scanline.nes

mmc3_irq_test_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_irq_tests/1.Clocking.nes

mmc3_irq_test_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_irq_tests/2.Details.nes

mmc3_irq_test_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_irq_tests/3.A12_clocking.nes

mmc3_irq_test_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_irq_tests/4.Scanline_timing.nes

mmc3_irq_test_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_irq_tests/5.MMC3_rev_A.nes

mmc3_irq_test_6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_irq_tests/6.MMC3_rev_B.nes

mmc3_irq_tests: mmc3_irq_test_1 mmc3_irq_test_2 mmc3_irq_test_3 mmc3_irq_test_4 mmc3_irq_test_6

mmc3_test_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test/1-clocking.nes

mmc3_test_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test/2-details.nes

mmc3_test_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test/3-A12_clocking.nes

mmc3_test_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test/4-scanline_timing.nes

mmc3_test_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test/5-MMC3.nes

mmc3_test_6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test/6-MMC6.nes

[parallel]
mmc3_tests: mmc3_test_1 mmc3_test_2 mmc3_test_3 mmc3_test_4 mmc3_test_5

mmc3_test2_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test_2/rom_singles/1-clocking.nes

mmc3_test2_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test_2/rom_singles/2-details.nes

mmc3_test2_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test_2/rom_singles/3-A12_clocking.nes

mmc3_test2_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test_2/rom_singles/4-scanline_timing.nes

mmc3_test2_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test_2/rom_singles/5-MMC3.nes

mmc3_test2_6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/mmc3_test_2/rom_singles/6-MMC3_alt.nes

[parallel]
mmc3_test2: mmc3_test2_1 mmc3_test2_2 mmc3_test2_3 mmc3_test2_4 mmc3_test2_5 mmc3_test2_6

mmc3: mmc3_tests mmc3_test2 mmc3_irq_tests

passed_mapper: mmc3

[parallel]
passed_cpu_tests: cpu-test instr_misc instr_test-v5 instr_test-v3 instr_timing cpu_dummy_reads cpu_dummy_writes cpu_exec_space cpu_reset cpu_timing_test6 nestest branch_timing_tests cpu_interrupts_v2

[parallel]
passed_ppu_tests: ppu_vbl_nmi vbl_nmi_timing oam_read oam_stress ppu_open_bus ppu_read_buffer sprite_hit_tests sprite_overflow_tests scanline

[parallel]
passed_apu_tests: apu_mixer apu_reset apu_test

todo_tests: sprdma_and_dmc_dma dmc_dma_during_read4

[parallel]
passed: unit-test passed_cpu_tests passed_ppu_tests passed_apu_tests

wasm-debug-build:
    cd nes_web && wasm-pack build --release

web-start: wasm-debug-build
    cd www && NODE_OPTIONS=--openssl-legacy-provider pnpm run start

test-cov:
    cd nes_core; cargo tarpaulin --skip-clean --out Html --engine llvm --output-dir /tmp/tarpaulin-output-dir
    xdg-open file:///tmp/tarpaulin-output-dir/tarpaulin-report.html

test-cov-text:
    cd nes_core; cargo tarpaulin --skip-clean --stderr --out Markdown --engine llvm --output-dir /tmp/tarpaulin-output-dir
