nes_cpu_test := "/tmp/nes-sim-target/release/nes_cpu_test"

cpu-test: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f nes_cpu_test/src/6502_functional_test.bin

instr_misc: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_misc/instr_misc.nes

instr_test-v3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_test-v3/all_instrs.nes

instr_test-v5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_test-v5/all_instrs.nes

nes_instr_test1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/01-implied.nes
nes_instr_test2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/02-immediate.nes
nes_instr_test3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/03-zero_page.nes
nes_instr_test4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/04-zp_xy.nes
nes_instr_test5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/05-absolute.nes
nes_instr_test6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/06-abs_xy.nes
nes_instr_test7: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/07-ind_x.nes
nes_instr_test8: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/08-ind_y.nes
nes_instr_test9: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/09-branches.nes
nes_instr_test10: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/10-stack.nes
nes_instr_test11: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nes_instr_test/rom_singles/11-special.nes

[parallel]
nes_instr_test: nes_instr_test1 nes_instr_test2 nes_instr_test3 nes_instr_test4 nes_instr_test5 nes_instr_test6 nes_instr_test7 nes_instr_test8 nes_instr_test9 nes_instr_test10 nes_instr_test11

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
    cargo test --lib --workspace

instr_timing: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/instr_timing/instr_timing.nes

ppu_vbl_nmi: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/ppu_vbl_nmi/ppu_vbl_nmi.nes

nmi_sync: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/nmi_sync/demo_ntsc.nes

build_nes_cpu_test:
    cargo build -p nes_cpu_test --release

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
    timeout 17 {{ nes_cpu_test }} --quiet -f ../nes-test-roms/cpu_timing_test6/cpu_timing_test.nes

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
apu_reset: apu_reset_1 apu_reset_2 apu_reset_3 apu_reset_4 apu_reset_5 apu_reset_6 dmc_dma_during_read4_4 dmc_dma_during_read4_5

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
mmc3_tests: mmc3_test_1 mmc3_test_2 mmc3_test_3 mmc3_test_4 mmc3_test_5 mmc3_test_6

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

bntest-h: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/bntest/bntest_h.nes

bntest-v: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/bntest/bntest_v.nes

bntest-aorom: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/bntest/bntest_aorom.nes

[parallel]
bntest: bntest-h bntest-v bntest-aorom

mmc1-a12: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f ../nes-test-roms/MMC1_A12/mmc1_a12.nes

vrc21-s1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest21s1.nes

vrc21-s2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest21s2.nes

vrc22: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest22.nes

vrc23-s1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest23s1.nes

vrc23-s2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest23s2.nes

vrc23-s3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest23s3.nes

vrc25-s1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest25s1.nes

vrc25-s2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest25s2.nes

vrc25-s3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f  test-roms/vrctests/vrctest25s3.nes

[parallel]
vrc2-and-4-roms: vrc21-s1 vrc21-s2 vrc22 vrc23-s1 vrc23-s2 vrc23-s3 vrc25-s1 vrc25-s2 vrc25-s3

# ---- Imported from tetanes/tetanes-core/test_roms --------------------------
# Deduplicated against ../nes-test-roms and test-roms by iNES file content
# (SHA-1), not by name: every ROM below is byte-distinct from anything already
# hosted. Assertion schemes follow tetanes-core: self-reporting ROMs use the
# $DE $B0 $61 signature at $6000 (MonitorTestStatus plugin); visual-only ROMs
# assert a blessed rendered-frame snapshot under nes_cpu_test/src/png-exps/
# (PngFrameMatch), at frame numbers quoted from
# tetanes-core/test_roms/ppu/tests.json.

imported_apu_misc_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/apu/apu_env.nes

imported_apu_misc_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/apu/dmc_pitch.nes

imported_apu_misc_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/apu/lin_ctr.nes

imported_apu_misc_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/apu/phase_reset.nes

imported_apu_misc_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/apu/sweep_cutoff.nes

imported_apu_misc_6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/apu/sweep_sub.nes

[parallel]
imported_apu_misc: imported_apu_misc_1 imported_apu_misc_2 imported_apu_misc_3 imported_apu_misc_4 imported_apu_misc_5 imported_apu_misc_6

imported_cpu_misc_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/cpu/exec_space_apu.nes

imported_cpu_misc_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/cpu/exec_space_ppuio.nes

imported_cpu_misc_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/cpu/flag_concurrency.nes

[parallel]
imported_cpu_misc: imported_cpu_misc_1 imported_cpu_misc_2 imported_cpu_misc_3

spr_hit_extra_1: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_alignment.nes

spr_hit_extra_2: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_basics.nes

spr_hit_extra_3: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_corners.nes

spr_hit_extra_4: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_double_height.nes

spr_hit_extra_5: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_flip.nes

spr_hit_extra_6: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_left_clip.nes

spr_hit_extra_7: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_right_edge.nes

spr_hit_extra_8: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/spr_hit_screen_bottom.nes

[parallel]
spr_hit_extra: spr_hit_extra_1 spr_hit_extra_2 spr_hit_extra_3 spr_hit_extra_4 spr_hit_extra_5 spr_hit_extra_6 spr_hit_extra_7 spr_hit_extra_8

ppu_240pee: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/_240pee.nes

ppu_color: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/color.nes

ntsc_torture: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/ntsc_torture.nes

ppu_palette: build_nes_cpu_test
    {{ nes_cpu_test }} --quiet -f test-roms/ppu/palette.nes

[parallel]
imported_ppu_visual: ppu_240pee ppu_color ntsc_torture ppu_palette

# Known-failing imports, kept runnable but excluded from passed_* groups until
# the core implements what they probe:

# blargg's forum APU tests: poll the APU frame IRQ flag with BVC loops the
# current frame sequencer never satisfies; the dead-loop detector exits 1.
apu_forum_test_1: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_1.nes

apu_forum_test_2: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_2.nes

apu_forum_test_3: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_3.nes

apu_forum_test_4: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_4.nes

apu_forum_test_5: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_5.nes

apu_forum_test_6: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_6.nes

apu_forum_test_7: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_7.nes

apu_forum_test_8: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_8.nes

apu_forum_test_9: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_9.nes

apu_forum_test_10: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/test_10.nes

[parallel]
apu_forum_tests: apu_forum_test_1 apu_forum_test_2 apu_forum_test_3 apu_forum_test_4 apu_forum_test_5 apu_forum_test_6 apu_forum_test_7 apu_forum_test_8 apu_forum_test_9 apu_forum_test_10

# Pitch/env/sweep audio tests: no self-reporting protocol (upstream tetanes
# leaves them ignored too); run and compare the captured audio by ear/waveform.
audio_compare_manual_1: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/noise_pitch.nes

audio_compare_manual_2: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/square_pitch.nes

audio_compare_manual_3: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/apu/triangle_pitch.nes

[parallel]
audio_compare_manual: audio_compare_manual_1 audio_compare_manual_2 audio_compare_manual_3

# Zapper light-gun tests: need Zapper input (light sense + trigger on $4017),
# which nes_core does not emulate yet.
zapper_flip: build_nes_cpu_test
    timeout 10 {{ nes_cpu_test }} --quiet -f test-roms/input/zapper_flip.nes

zapper_light: build_nes_cpu_test
    timeout 10 {{ nes_cpu_test }} --quiet -f test-roms/input/zapper_light.nes

zapper_stream: build_nes_cpu_test
    timeout 10 {{ nes_cpu_test }} --quiet -f test-roms/input/zapper_stream.nes

zapper_trigger: build_nes_cpu_test
    timeout 10 {{ nes_cpu_test }} --quiet -f test-roms/input/zapper_trigger.nes

[parallel]
zapper_tests: zapper_flip zapper_light zapper_stream zapper_trigger

# MMC3 CHR-RAM banking probe: spins waiting on pattern data it wrote to CHR
# RAM, which the TxROM mapper does not allocate for CHR-RAM carts yet.
big_chr_ram: build_nes_cpu_test
    timeout 15 {{ nes_cpu_test }} --quiet -f test-roms/mapper/m004_txrom/big_chr_ram.nes

# All 18 imported ROMs not yet passing: known-failing or manual-only, kept as
# a single runnable checklist. Each graduates into the passed_* aggregates as
# nes_core gains what it probes (frame-IRQ timing, Zapper input, MMC3
# CHR-RAM banking; audio-compare tests stay manual).
[parallel]
todo_tests: apu_forum_tests audio_compare_manual zapper_tests big_chr_ram

[parallel]
passed_mapper: mmc3 bntest mmc1-a12 vrc2-and-4-roms

[parallel]
passed_cpu_tests: cpu-test instr_misc instr_test-v5 instr_test-v3 instr_timing cpu_dummy_reads cpu_dummy_writes cpu_exec_space cpu_reset nestest branch_timing_tests nes_instr_test cpu_timing_test6 cpu_interrupts_v2 imported_cpu_misc

[parallel]
passed_ppu_tests: oam_read oam_stress ppu_open_bus ppu_read_buffer sprite_hit_tests sprite_overflow_tests scanline sprdma_and_dmc_dma vbl_nmi_timing ppu_vbl_nmi spr_hit_extra imported_ppu_visual

[parallel]
passed_apu_tests: apu_mixer apu_reset apu_test dmc_dma_during_read4 imported_apu_misc

[parallel]
passed_rom_tests: passed_cpu_tests passed_ppu_tests passed_apu_tests passed_mapper

[parallel]
passed: unit-test passed_rom_tests

wasm-debug-build:
    cd nes_web && wasm-pack build --release

web-start: wasm-debug-build
    cd www && NODE_OPTIONS=--openssl-legacy-provider pnpm run start

test-cov:
    cd nes_core; cargo tarpaulin --skip-clean --out Html --engine llvm --output-dir /tmp/tarpaulin-output-dir
    xdg-open file:///tmp/tarpaulin-output-dir/tarpaulin-report.html

test-cov-text:
    cd nes_core; cargo tarpaulin --skip-clean --stderr --out Markdown --engine llvm --output-dir /tmp/tarpaulin-output-dir

mount_nes_dev_wiki:
    fuse-zip ../nesdevwiki.zip nesdev-wiki/
