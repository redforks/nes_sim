//! TCP protocol for NES emulator MCP communication
//!
//! Messages are length-prefixed with 4-byte big-endian u32 followed by JSON payload.

use serde::{Deserialize, Serialize};

/// Request type from MCP server to nes_cpu_test
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "params")]
pub enum Request {
    /// Read memory range
    ReadMemory { start: u16, end: u16 },
    /// Step execution
    Step,
    /// Forward execution until VBlank (scanline 241, dot 1)
    ForwardToVblank,
    /// Reset the machine
    Reset,
    /// Get current status
    GetStatus,
    /// Get CPU register state
    GetCpuRegisters,
    /// Get APU status
    GetApuStatus,
    /// Get PPU state dump
    GetPpuStatus,
    /// Read OAM memory
    ReadOam,
    /// Read nametable memory (index: 0-3 for specific, 255 for all)
    ReadNametable { index: u8 },
}

/// Response type from nes_cpu_test to MCP server
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "result")]
pub enum Response {
    /// Memory read result with hexdump-formatted data
    MemoryData { data: String },
    /// Step execution result
    StepResult { stopped: bool },
    /// Forward to VBlank result
    ForwardToVblankResult { ticks: u64 },
    /// Reset acknowledgment
    ResetDone,
    /// Current machine status
    Status { status: MachineStatus },
    /// CPU register state
    CpuRegisters { registers: CpuRegisters },
    /// APU status
    ApuStatus { status: ApuStatus },
    /// PPU state dump
    PpuStatus { status: String },
    /// OAM memory data
    OamData { data: String },
    /// Nametable memory data
    NametableData { data: String },
    /// Error response
    Error { message: String },
}

/// Machine status information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MachineStatus {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: u8,
    pub sp: u8,
    pub cycles: u64,
}

/// CPU register state with decoded flags
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CpuRegisters {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub status: u8,
    pub cycles: u64,
    // Decoded flags for readability
    pub flag_n: bool,
    pub flag_v: bool,
    pub flag_d: bool,
    pub flag_i: bool,
    pub flag_z: bool,
    pub flag_c: bool,
}

/// APU channel and status information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApuStatus {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    pub frame_irq_pending: bool,
    pub dmc_irq_pending: bool,
}

impl Default for MachineStatus {
    fn default() -> Self {
        Self {
            pc: 0,
            a: 0,
            x: 0,
            y: 0,
            p: 0,
            sp: 0,
            cycles: 0,
        }
    }
}

/// Format memory data as hexdump -C style
pub fn format_hexdump(data: &[u8], start_addr: u16) -> String {
    let mut output = String::new();
    let mut addr = start_addr;

    for chunk in data.chunks(16) {
        // Address
        output.push_str(&format!("{:08x}  ", addr));

        // Hex bytes
        for (i, byte) in chunk.iter().enumerate() {
            output.push_str(&format!("{:02x} ", byte));
            if i == 7 {
                output.push(' ');
            }
        }

        // Padding for incomplete lines
        for i in chunk.len()..16 {
            output.push_str("   ");
            if i == 7 {
                output.push(' ');
            }
        }

        // ASCII representation
        output.push_str(" |");
        for byte in chunk {
            if byte.is_ascii_graphic() || *byte == b' ' {
                output.push(*byte as char);
            } else {
                output.push('.');
            }
        }
        output.push_str("|\n");

        addr = addr.wrapping_add(16);
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_hexdump() {
        let data = vec![
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
            0x0e, 0x0f, b'H', b'e', b'l', b'l', b'o', 0xff, 0x7f, 0x00,
        ];
        let result = format_hexdump(&data, 0x8000);

        assert!(result.contains("00008000"));
        assert!(result.contains("00 01 02 03"));
        assert!(result.contains("Hello"));
    }
}
