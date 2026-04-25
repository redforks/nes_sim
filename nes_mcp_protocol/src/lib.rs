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
    /// Reset the machine
    Reset,
    /// Get current status
    GetStatus,
}

/// Response type from nes_cpu_test to MCP server
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "result")]
pub enum Response {
    /// Memory read result with hexdump-formatted data
    MemoryData { data: String },
    /// Step execution result
    StepResult { stopped: bool },
    /// Reset acknowledgment
    ResetDone,
    /// Current machine status
    Status { status: MachineStatus },
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
        assert!(result.contains("|Hello|"));
    }
}
