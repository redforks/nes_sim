# NES Core Test Coverage Analysis

## Overview
This document provides a comprehensive analysis of the `nes_core` crate to identify what needs testing to reach 90% coverage. Currently, only 44 tests exist covering peripheral modules, while the main CPU execution and machine control loops have minimal to zero coverage.

---

## 1. Module Analysis: What Needs Testing

### A. **src/cpu.rs** (Currently ~2% coverage)

#### Module Purpose
- Central 6502 CPU emulator implementing cycle-accurate instruction execution
- Manages CPU state (registers A, X, Y, PC, SP, status flags)
- Orchestrates IRQ/NMI interrupt handling
- Coordinates with Plugin system for event hooks

#### Key Structures & Functions to Test

**Cpu struct (384 lines, ~30 public methods)**
```rust
pub struct Cpu {
    pub a: u8,               // Accumulator
    pub x: u8,               // X register
    pub y: u8,               // Y register
    pub pc: u16,             // Program counter
    pub sp: u8,              // Stack pointer
    pub status: u8,          // Status flags
    mcu: Box<dyn Mcu>,       // Memory interface
    irq_pending: bool,
    change_irq_flag_pending: Option<bool>,
    remain_clocks: u16,      // Cycle countdown
    is_halt: bool,
}
```

**Critical Methods to Test:**
1. **`new(mcu: Box<dyn Mcu>) -> Cpu`** - Constructor, initializes with proper defaults
2. **`reset()`** - Reads reset vector (0xFFFC) and sets PC
3. **`clock_tick<T: Plugin>(plugin: &mut T) -> ExecuteResult`** - Main execution loop
   - PPU ticking (3x per CPU cycle)
   - Interrupt handling (IRQ/NMI)
   - Instruction execution
   - Plugin hooks
4. **`nmi()`** - Non-maskable interrupt handling
   - Pushes PC and status to stack
   - Reads NMI vector (0xFFFA)
5. **`set_irq(enabled: bool)`** - IRQ pending flag management
6. **`flag(flag: Flag) -> bool`** - Flag reading
7. **`set_flag(flag: Flag, v: bool)`** - Flag manipulation (all 8 flags)
8. **`push_stack()` / `pop_stack()`** - Stack operations
9. **`read_byte()` / `write_byte()`** - Memory access via MCU
10. **`read_word()` / `read_zero_page_word()`** - 16-bit memory access

**Flag enum** - Test all 8 flags:
- Carry (0x01), Zero (0x02), InterruptDisabled (0x04), Decimal (0x08)
- Break (0x10), NotUsed (0x20), Overflow (0x40), Negative (0x80)

**ExecuteResult enum** - Test all three variants:
- `Continue`, `Stop(u8)`, `ShouldReset`

**Helper Functions:**
- `is_cross_page(a: u16, b: u16) -> bool`
- `extra_tick_if_cross_page(a: u16, b: u16) -> u8`

#### Critical Code Paths (Not Yet Tested)
```rust
// IRQ/NMI handling with interrupt delay
- IRQ execution while I flag is clear
- NMI always executes (cannot be disabled)
- CLI delay: pending_set_interrupt_disabled_flag behavior
- IRQ with I flag set (should NOT execute)

// Clock ticking
- PPU ticking 3x per CPU cycle
- remain_clocks countdown mechanism
- is_halt state handling

// Stack operations with wraparound
- Stack pointer wraparound (0xFF -> 0x00)
- PC split into high/low bytes when pushed

// Interrupt vectors
- NMI vector at 0xFFFA (lower byte first)
- IRQ vector at 0xFFFE (lower byte first)
- Reset vector at 0xFFFC (lower byte first)
```

#### Coverage Gaps
- **~98% untested**: All CPU methods except basic construction
- Zero coverage on interrupt handling logic
- Zero coverage on flag operations beyond basic get/set
- Zero coverage on stack push/pop operations
- Zero coverage on cycle timing

---

### B. **src/cpu/instruction.rs** (542 lines, ~40 public functions)

#### Module Purpose
- Implements all 6502 instruction handlers
- Returns cycle counts for each instruction
- Handles operand size variations (1, 2, or 3 byte instructions)

#### Key Functions to Test (in suggested testing order)

**Basic Arithmetic & Logic (9 functions):**
1. `new_transfer<S, D>(dest: D, src: S)` - LDA, LDX, LDY, TAX, TAY, TSX, TXA, TYA
2. `new_transfer_no_touch_flags<S, D>(dest: D, src: S)` - STA, STX, STY
3. `new_adc<D>(dest: D)` - Add with carry (with overflow flag logic)
4. `new_sbc<D>(dest: D)` - Subtract with carry
5. `new_and<D>(dest: D)` - Bitwise AND
6. `new_ora<D>(dest: D)` - Bitwise OR
7. `new_eor<D>(dest: D)` - Bitwise XOR
8. `new_inc<D>(dest: D)` - Increment (register or memory)
9. `new_dec<D>(dest: D)` - Decrement (register or memory)

**Comparison & Flags (6 functions):**
10. `new_cmp<R, M>(r: R, m: M)` - Compare (all three: A, X, Y)
11. `new_clear_bit(dest: FlagAddr)` - CLC, CLD, CLI, CLV
12. `new_set_bit(dest: FlagAddr)` - SEC, SED, SEI
13. `new_bit<D>(dest: D)` - BIT instruction (test bits, set V and N)
14. `new_condition_branch<R>(offset, flag, negate)` - Branch instructions
15. `pending_set_interrupt_disabled_flag()` - CLI/SEI delay mechanism

**Stack Operations (5 functions):**
16. `new_pha()` - Push accumulator
17. `new_php()` - Push processor status
18. `new_pla()` - Pull accumulator
19. `new_plp()` - Pull processor status (with I flag special handling)
20. `new_brk()` - Break (push PC+2 and status)

**Control Flow (5 functions):**
21. `new_jmp(addr: u16)` - Unconditional jump
22. `new_indirect_jmp(offset: u16)` - Indirect JMP (JMP ($nnnn))
23. `new_jsr(addr: u16)` - Jump to subroutine
24. `new_rts()` - Return from subroutine
25. `new_rti()` - Return from interrupt

**Bit Shifting (4 functions):**
26. `new_asl<D>(dest: D)` - Arithmetic shift left
27. `new_lsr<D>(dest: D)` - Logical shift right
28. `new_rol<D>(dest: D)` - Rotate left (through carry)
29. `new_ror<D>(dest: D)` - Rotate right (through carry)

**Undocumented Instructions (8 functions):**
30. `new_anc(Literal(v))` - AND immediate with carry copy
31. `new_alr(Literal(v))` - AND + LSR
32. `new_arr(Literal(v))` - AND + ROR
33. `new_sbx(Literal(v))` - Subtract X from A
34. `new_aso<D>(dest: D)` - ASL + ORA
35. `new_rla<D>(dest: D)` - ROL + AND
36. `new_lse<D>(dest: D)` - LSR + EOR
37. `new_rra<D>(dest: D)` - ROR + ADC
38. `new_sax<D>(dest: D)` - Store A AND X
39. `new_lax<D>(dest: D)` - Load A AND X
40. `new_dcp<D>(dest: D)` - DEC + CMP
41. `new_isc<D>(dest: D)` - INC + SBC

**Special (4 functions):**
42. `new_nop()` - No operation
43. `new_nop_with_addr<D>(dest: D)` - NOP with addressing mode
44. `new_hlt()` - Halt (unofficial)
45. `new_all<R, D>(name: &str, r: R, dest: D)` - SHY, SHX (undocumented)

#### Critical Testing Paths

**Flag-affecting instructions (test with various flag states):**
- Verify Zero flag set when result == 0
- Verify Negative flag set when result & 0x80 != 0
- Verify Carry flag on overflow/borrow
- Verify Overflow flag on signed arithmetic

**ADC specific (complex overflow logic):**
```
Overflow = !(A ^ operand) & (A ^ result) & 0x80
```

**SBC (implemented as ADC with inverted operand):**
```
SBC = ADC with ~operand
```

**Rotate instructions (carry flag as 9th bit):**
- ROL: C7 6-0 <- new C
- ROR: 7 C 6-0 -> C

**Branch instructions:**
- Branch taken: +2 cycles (if no page cross)
- Branch not taken: +2 cycles
- Page cross when branch taken: +1 extra cycle

**Interrupt handling:**
- CLI: Sets I flag to 0 after current instruction
- SEI: Sets I flag to 1 immediately

#### Coverage Gaps
- **~95% untested**: Almost all 40+ instruction handlers
- Zero tests for ADC overflow calculation
- Zero tests for rotate/shift carry handling
- Zero tests for undocumented instruction behavior
- Zero tests for conditional branches

---

### C. **src/cpu/addressing.rs** (388 lines, 15+ addressing modes)

#### Current Coverage: ~40% (15 existing tests covering formatting and basic addressing)

**Already Tested:**
- Format output for all addressing modes
- Basic addressing calculations

**Gaps:**
- Page-crossing tick penalty for AbsoluteX/AbsoluteY
- Zero-page register wraparound (e.g., $FF + X wraps to $00)
- Indirect addressing edge cases
- IndirectX/IndirectY page boundary behavior

---

### D. **src/machine.rs** (56 lines, 0% coverage)

#### Module Purpose
- Wraps CPU with a Plugin for extensibility
- Implements frame execution timing based on elapsed milliseconds
- Connects CPU to MCU and rendering system

#### Key Structures & Functions

**Constants:**
```rust
pub const CYCLES_PER_FRAME: f64 = 29780.5;  // 262 scanlines * 113.67 dots (CPU cycles)
pub const CYCLES_PER_MS: f64 = 499.675;    // CYCLES_PER_FRAME / 16.68ms * 1000
pub const V_BLANK_CYCLES: f64 = 2273.3333333;  // V-blank duration
```

**Machine<P> struct:**
```rust
pub struct Machine<P> {
    cpu: Cpu,
    p: P,  // Plugin for extensibility
}
```

**Key Methods to Test:**
1. **`new(mcu: Box<dyn Mcu>) -> Machine<EmptyPlugin>`**
   - Creates machine with EmptyPlugin (no-op hooks)
2. **`with_plugin<P>(p: P, mcu: Box<dyn Mcu>) -> Machine<P>`**
   - Creates machine with custom plugin
3. **`process_frame(ms: f64) -> (&RgbaImage, ExecuteResult)`**
   - Converts milliseconds to CPU cycles
   - Runs ticks and returns rendered frame
   - Tests: frame timing calculation, integration with PPU
4. **`run_ticks(ticks: u32) -> ExecuteResult`**
   - Executes specified number of CPU cycles
   - Tests: cycle loop, interrupt propagation, halt conditions
5. **`reset()`** - Delegates to CPU reset
6. **`set_pc(pc: u16)`** - Directly sets program counter (debugging aid)

#### Critical Testing Scenarios

**Timing accuracy:**
```rust
// 16.67ms (one frame at 60 FPS)
let cycles = (16.67 * CYCLES_PER_MS) as u32;  // Should be ~8322 cycles
process_frame(16.67);

// Half frame (8.33ms)
let cycles = (8.33 * CYCLES_PER_MS) as u32;   // Should be ~4161 cycles
```

**Interrupt propagation:**
- CPU halt signals should stop frame execution
- Exit codes should propagate through to caller

**Plugin integration:**
- Plugin::start() called before each instruction
- Plugin::end() called after each instruction
- Plugin::should_stop() can halt execution

#### Coverage Gaps
- **100% untested**: All Machine methods
- Zero tests on frame timing calculations
- Zero tests on cycle-to-millisecond conversion accuracy
- Zero tests on interrupt propagation
- Zero tests on plugin integration

---

### E. **src/mcu.rs** (39 lines, 0% coverage)

#### Module Purpose
- Trait definitions for memory-mapped I/O abstraction
- Separates CPU from specific hardware implementation

#### Key Traits & Functions

**Mcu trait (8 required/optional methods):**
```rust
pub trait Mcu {
    fn read(&self, address: u16) -> u8;           // REQUIRED: Read byte from address
    fn write(&mut self, address: u16, value: u8); // REQUIRED: Write byte to address
    fn get_ppu(&mut self) -> &mut dyn PpuTrait;   // Optional: Access PPU (panics if not impl)
    fn get_machine_mcu(&mut self) -> &mut dyn MachineMcu;  // Optional: Access render (panics if not impl)
    fn request_irq(&self) -> bool;                // Optional: IRQ request status
    fn tick_ppu(&mut self) -> bool;               // Optional: Tick PPU, returns NMI signal
}
```

**MachineMcu trait (1 required method):**
```rust
pub trait MachineMcu {
    fn render(&mut self) -> &RgbaImage;           // REQUIRED: Get current rendered frame
}
```

#### Supporting Types

**RamMcu<N>** (tested separately, has 1 test)
- Generic RAM implementation for addresses with fixed offset
- Used for 2KB internal RAM and APU region

#### Coverage Gaps
- **0% direct tests** (tested indirectly through NesMcu)
- Could test trait contract with mock implementations
- Optional method panic behavior not tested

---

### F. **src/nes/nes_mcu.rs** (111 lines, 0% coverage)

#### Module Purpose
- Concrete Mcu implementation for full NES hardware
- Routes address reads/writes to appropriate devices:
  - 0x0000-0x1FFF: Internal 2KB RAM (mirrored)
  - 0x2000-0x3FFF: PPU memory-mapped registers
  - 0x4000-0x401F: APU/IO registers
  - 0x4020-0xFFFF: Cartridge (PRG ROM, mapper logic)

#### Key Structures & Functions

**NesMcu struct:**
```rust
pub struct NesMcu {
    lower_ram: LowerRam,           // 2KB internal RAM
    ppu: Ppu,                      // Picture Processing Unit
    after_ppu: RamMcu<0x20>,       // APU/IO region ($4000-$401F)
    cartridge: Box<dyn Cartridge>, // Mapper + ROM
    frame_counter_interrupt: Cell<bool>,  // APU frame counter IRQ
    dmc_interrupt: Cell<bool>,            // APU DMC channel IRQ
    nmi_pending: Cell<bool>,              // NMI from PPU
}
```

**Key Methods to Test:**
1. **`build(file: &INesFile) -> impl Mcu`**
   - Creates NesMcu from iNES file
   - Initializes cartridge with mapper
   - Sets PPU mirroring based on header
   - Tests: Proper device initialization, mirroring setup

2. **`read(address: u16) -> u8`** (54 lines of logic)
   - Memory address routing for reads
   - Tests: Each address range routes to correct device
   - Test PPU register mirroring ($2000-$3FFF mirrors every $20 bytes)
   - Test frame counter interrupt flag clearing (reading 0x4015)

3. **`write(address: u16, value: u8)`** (18 lines of logic)
   - Memory address routing for writes
   - Tests: Each address range routes to correct device
   - Test PPU_DMA trigger at $4014
   - Test interrupt control: $4015, $4017

4. **`ppu_dma(address: u8)`** (8 lines)
   - DMA transfer from CPU memory to PPU OAM
   - Tests: 256-byte copy from (address << 8) to OAM

5. **Trait implementations:**
   - `request_irq()`: Returns IRQ from frame counter or DMC
   - `tick_ppu()`: Advances PPU, returns NMI signal
   - `get_ppu()`: Returns PPU reference
   - `get_machine_mcu()`: Returns self for rendering

#### Critical Testing Scenarios

**Address routing:**
```rust
// Test internal RAM mirroring
write(0x0000, 0x42);
assert_eq!(read(0x0800), 0x42);  // Mirrored at 0x0800
assert_eq!(read(0x1000), 0x42);  // Mirrored at 0x1000
assert_eq!(read(0x1800), 0x42);  // Mirrored at 0x1800

// Test PPU register mirroring
write(0x2000, 0xFF);  // PPU_CTRL
assert_eq!(read(0x2020), 0xFF);  // Mirrors every 0x20 bytes
assert_eq!(read(0x2040), 0xFF);

// Test cartridge reads
let data = read(0x8000);  // PRG ROM space
```

**DMA transfer:**
```rust
// Setup: Write sprite data at CPU $0100-$01FF
for addr in 0x0100..0x0200 {
    write(addr, (addr & 0xFF) as u8);
}
// Trigger DMA: write to $4014 with page number
write(0x4014, 0x01);  // Copy $0100-$01FF to PPU OAM
// Verify PPU received it via get_ppu().oam_dma()
```

**Interrupt signaling:**
```rust
// PPU generates NMI
assert!(tick_ppu() == true);  // Should propagate NMI
assert!(request_irq() == false);  // No CPU IRQ from PPU

// Frame counter generates IRQ
write(0x4017, 0x00);  // Enable frame counter
// ... advance cycles ...
write(0x4015, 0);  // Clear frame counter interrupt flag
assert!(request_irq() == true);  // Should be true before clear
```

#### Coverage Gaps
- **100% untested**: All NesMcu methods
- Zero tests on address range routing
- Zero tests on PPU register mirroring
- Zero tests on DMA functionality
- Zero tests on interrupt coordination
- Zero tests on cartridge integration

---

### G. **src/nes/apu.rs** (358 lines, 0% coverage)

#### Module Purpose
- Audio Processing Unit (APU) abstraction
- Trait-based design allows different audio implementations
- Handles pulse channels, triangle, noise, DMC with envelope/length counter logic
- Frame counter interrupt generation

#### Key Structures & Traits

**Data structures:**
```rust
pub struct Sweep { /* shift register for pulse frequency sweep */ }
pub struct DutyCycle { /* duty cycle, volume, halt flag */ }
pub struct LengthCounterLoad { /* length counter and timer */ }
pub struct LinearCounterControl { /* triangle linear counter */ }
pub struct NoiseEnvelop { /* noise volume envelope */ }
pub struct NoisePeriod { /* noise period/mode */ }
pub struct NoiseLength { /* noise length counter */ }
pub struct DmcIRQLoopFreq { /* DMC control flags */ }
pub struct ControlFlags { /* channel enable flags */ }
pub struct APUStatus { /* active channel status */ }
pub struct FrameCounter { /* frame counter mode and step */ }
```

**Driver traits (audio backend abstraction):**
```rust
pub trait PulseDriver { /* Pulse 1/2 control */ }
pub trait TriangleDriver { /* Triangle channel control */ }
pub trait NoiseDriver { /* Noise channel control */ }
pub trait DmcDriver { /* DMC (delta modulation) control */ }
pub trait APUControllerDriver { /* Master control */ }
```

**Channel implementations:**
```rust
struct PulseChannel<D: PulseDriver> { /* Pulse 1 or 2 */ }
struct TriangleChannel<D: TriangleDriver> { /* Triangle */ }
struct NoiseChannel<D: NoiseDriver> { /* Noise */ }
struct DmcChannel<D: DmcDriver> { /* DMC */ }
```

#### Critical Functions to Test

**Data structure conversions:**
1. `Sweep::from(u8)` and `Sweep::to_u8()` - Bitfield operations
2. `DutyCycle::from(u8)` and `DutyCycle::to_u8()`
3. `LinearCounterControl::from(u8)` and `LinearCounterControl::to_u8()`
4. `LengthCounterLoad` helper methods:
   - `length_count()` - Extract length counter from high byte
   - `timer()` - Combine high/low bytes into 11-bit timer

**Channel write operations:**
- Each register write (0-3) calls different driver method
- Tests register decode and driver interface

**APU composition:**
```rust
pub fn new<PD, TD, ND, DD, CD>(
    pulse_driver: PD,
    triangle_driver: TD,
    noise_driver: ND,
    dmc_driver: DD,
    controller_driver: CD,
) -> impl Mcu
```

#### Complex Features to Test

**Length counter reload:**
```rust
// Writing high byte of length counter load triggers reload
write_high_byte(0xF0);  // Bits 7-3 contain length counter index
// Driver.set_length_counter_load() called with:
// - low_byte: 8-bit timer value
// - high_byte: 5-bit length counter index
```

**Linear counter (Triangle):**
```rust
// Writing to linear counter control
// Bit 7: Reload flag
// Bits 6-0: Counter value
```

**Envelope (Pulse & Noise):**
```rust
// Duty cycle (Pulse) byte:
// Bits 7-6: Duty cycle select
// Bit 5: Length counter halt / Envelope decay loop
// Bit 4: Constant volume flag
// Bits 3-0: Volume / Envelope decay rate
```

#### Coverage Gaps
- **100% untested**: All APU structures and driver trait integration
- Zero tests on register write decoding
- Zero tests on bitfield conversions
- Zero tests on timer/counter calculations
- Zero tests on envelope/sweep logic (if implemented)

---

## 2. Summary Table: Test Priorities

| Module | Lines | Pub Items | Current Coverage | Priority | Difficulty | Est. Tests |
|--------|-------|-----------|------------------|----------|-----------|-----------|
| cpu.rs | 617 | 30+ | ~2% | **CRITICAL** | Medium | 50-70 |
| instruction.rs | 542 | 40+ | ~0% | **CRITICAL** | Hard | 100-150 |
| machine.rs | 56 | 6 | 0% | **HIGH** | Easy | 15-20 |
| nes_mcu.rs | 111 | 5+ | 0% | **HIGH** | Medium | 30-40 |
| mcu.rs | 39 | 3 | 0% | MEDIUM | Easy | 10-15 |
| apu.rs | 358 | 30+ | 0% | MEDIUM | Hard | 40-60 |
| **TOTALS** | **1,723** | **110+** | **~5%** | | | **245-355** |

---

## 3. Existing Test Patterns & Approach

### Successfully Tested Modules (Reference Implementation)

**1. PPU Tests (10 tests)**
```rust
// Located in: src/nes/ppu.rs
// Pattern: Create test helper functions that setup complex state
fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
    (Ppu::default(), [0; 8192])
}

// Pattern: Test public methods with realistic scenarios
#[test]
fn palette_read_write() {
    let mut p = Palette::default();
    p.write(0x3f00, 15);
    assert_eq!(15, p.read(0x3f00));
}
```

**2. Addressing Mode Tests (15 tests)**
```rust
// Located in: src/cpu/addressing.rs
// Pattern: Test formatting, basic functionality, edge cases
#[test]
fn zero_page_x_wrapping() {
    // Tests that $FF + X wraps to zero page boundary
}

#[test]
fn absolute_x_page_crossing() {
    // Tests 1-cycle penalty for page crosses
}
```

**3. Mapper Tests (10 tests)**
```rust
// Located in: src/nes/mapper/mmc1.rs
// Pattern: Test state machine transitions
#[test]
fn shift_register_logic() {
    // Tests 5-bit shift register accumulation
}
```

**4. iNES Parser Tests (2 tests)**
```rust
// Located in: src/ines/parser.rs
// Pattern: Test with real ROM headers
#[test]
fn parse_valid_header() {
    let data = vec![0x4E, 0x45, 0x53, 0x1A, /* ...rest of header... */];
    let file = INesFile::new(data).unwrap();
    assert_eq!(file.header().prg_rom_pages, expected);
}
```

---

## 4. Suggested Test Structure

### Test Organization Pattern
```rust
// In each module with tests:
#[cfg(test)]
mod tests {
    use super::*;
    
    // 1. Helper structs for test state setup
    struct TestCpu;
    impl TestCpu {
        fn new_with_memory(program: &[u8]) -> Cpu {
            // Create test-friendly Cpu with mock MCU
        }
    }
    
    // 2. Group tests by functionality
    mod cpu_registers {
        // Tests for A, X, Y, PC, SP, status
    }
    
    mod cpu_interrupts {
        // Tests for IRQ, NMI, interrupt vectors
    }
    
    mod cpu_instructions {
        // Tests for each instruction category
    }
}
```

### Test Utility Functions Needed
```rust
// Create a simple test MCU (mock)
fn create_test_mcu(program: &[u8]) -> Box<dyn Mcu> {
    // RamMcu with program pre-loaded
}

// Assert CPU state helpers
fn assert_flags(cpu: &Cpu, expected_flags: u8) {
    assert_eq!(cpu.status, expected_flags);
}

fn assert_registers(cpu: &Cpu, a: u8, x: u8, y: u8) {
    assert_eq!(cpu.a, a);
    assert_eq!(cpu.x, x);
    assert_eq!(cpu.y, y);
}

fn assert_stack_contains(cpu: &Cpu, values: &[u8]) {
    for (i, &value) in values.iter().enumerate() {
        assert_eq!(cpu.peek_stack_at(i as u8), value);
    }
}
```

---

## 5. Recommended Testing Approach by Module

### Phase 1 (Critical): CPU Core (125-150 tests)
1. **Basic CPU Operations (20 tests)**
   - Constructor and initialization
   - Reset vector reading
   - All flag operations (set/clear/read)
   - Basic register operations

2. **Stack Operations (15 tests)**
   - Push/pop single bytes
   - PC push/pop (16-bit)
   - Status push/pop
   - Stack pointer wraparound

3. **Memory Access (15 tests)**
   - Byte read/write
   - Word read/write
   - Zero-page word reads (special boundary handling)
   - Same-page word reads

4. **Interrupt Handling (20 tests)**
   - NMI execution
   - IRQ with I flag clear
   - IRQ with I flag set (should not execute)
   - Interrupt vectors
   - CLI delay mechanism

5. **Instruction Execution (45 tests)**
   - Basic instruction fetch-decode-execute cycle
   - Cycle counting accuracy
   - ADC overflow calculations
   - Conditional branches (taken/not taken)
   - Undocumented instruction behavior

6. **Clock Ticking (15 tests)**
   - PPU ticking 3x per cycle
   - remain_clocks countdown
   - Halt state handling
   - Plugin hook execution

### Phase 2 (High): Machine & Hardware Integration (80-100 tests)
1. **Machine Frame Execution (20 tests)**
   - Frame timing calculations
   - Milliseconds to cycles conversion
   - Frame processing with rendering

2. **NesMcu Address Routing (30 tests)**
   - RAM mirroring verification
   - PPU register routing
   - Cartridge access
   - APU/IO register access

3. **DMA Transfer (15 tests)**
   - OAM DMA functionality
   - 256-byte copy correctness

4. **Interrupt Coordination (15 tests)**
   - NMI propagation
   - Frame counter IRQ
   - DMC IRQ
   - Multiple interrupt sources

### Phase 3 (Medium): APU & Advanced Features (40-60 tests)
1. **APU Register Decoding (25 tests)**
   - Bitfield conversions
   - Register writes to driver methods

2. **APU Channel Control (20 tests)**
   - Pulse channel envelope
   - Triangle linear counter
   - Noise period/envelope
   - DMC sample fetch

3. **Frame Counter (10 tests)**
   - Timing of frame counter steps
   - Interrupt generation

---

## 6. Quick Reference: Critical Test Cases

### CPU Examples (What to Test)

**ADC with Overflow:**
```rust
#[test]
fn adc_overflow_positive_plus_positive() {
    let mut cpu = create_test_cpu();
    cpu.a = 0x50;           // +80
    cpu.status = 0;         // C=0
    // Execute: ADC #0x50
    // Result: A=0xA0, V=1 (overflow), N=1
    assert!(cpu.flag(Flag::Overflow));
    assert!(cpu.flag(Flag::Negative));
}

#[test]
fn adc_overflow_negative_plus_negative() {
    let mut cpu = create_test_cpu();
    cpu.a = 0xD0;           // -48
    // Execute: ADC #0xB0 (-80)
    // Result: A=0x80, V=1 (overflow), N=1
    assert!(cpu.flag(Flag::Overflow));
}
```

**Branch Page Crossing:**
```rust
#[test]
fn branch_forward_with_page_cross() {
    let mut cpu = create_test_cpu();
    cpu.pc = 0x00FE;
    // Branch forward 2: crosses from $00FF to $0100
    // Should take +3 cycles (normal +2, +1 for page cross)
}
```

**NMI During CLI:**
```rust
#[test]
fn nmi_after_cli() {
    let mut cpu = create_test_cpu();
    // Execute: CLI (clears I flag)
    // NMI should NOT execute in next cycle
    // But should execute in cycle after that
}
```

---

## 7. Estimated Timeline & Coverage Improvement

| Phase | Tests | Est. Coverage | Time |
|-------|-------|---------------|------|
| Current | 44 | ~5% | - |
| After Phase 1 | 169 | ~25% | 2-3 weeks |
| After Phase 2 | 249 | ~60% | 3-4 weeks |
| After Phase 3 | 309+ | ~85-90% | 2-3 weeks |
| **Total** | **~350+** | **~90%+** | **7-10 weeks** |

---

## Key Insights

1. **CPU execution is nearly untested** - Despite 617 lines of CPU code, only ~2% is covered. This is the #1 priority.

2. **Instruction handlers are complex** - 40+ instruction functions with multiple addressing modes create ~500+ test cases needed for full coverage.

3. **Interrupt behavior is subtle** - CLI delay, IRQ vs NMI differences, and vector handling require careful edge case testing.

4. **APU is architectural** - 358 lines of APU code uses trait-based design that requires mock implementations to test effectively.

5. **Existing patterns are good** - The 44 existing tests show clear, readable patterns that can be scaled up for new tests.

6. **Quick wins exist** - Machine (56 lines) and mcu.rs (39 lines) can reach 80%+ coverage with 15-30 tests each.

