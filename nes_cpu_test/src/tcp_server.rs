//! TCP server for MCP communication
//!
//! Listens on port 28800 and handles requests from the MCP server.

use crate::image::{MachineWrapper, load_image};
use nes_core::ExecuteResult;
use nes_core::mcu::Mcu;
use nes_mcp_protocol::{Request, Response, format_hexdump};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;
use tracing::{error, info, warn};

const TCP_PORT: u16 = 28800;
const TCP_HOST: &str = "0.0.0.0";

/// Shared machine state
struct MachineState {
    machine: MachineWrapper,
    quiet: bool,
}

/// Handle a single client connection
async fn handle_client(mut socket: tokio::net::TcpStream, state: Arc<Mutex<MachineState>>) {
    let quiet = { state.lock().unwrap().quiet };
    if !quiet {
        info!("Client connected");
    }

    loop {
        // Read message length
        let mut len_buf = [0u8; 4];
        if let Err(e) = socket.read_exact(&mut len_buf).await {
            if e.kind() == std::io::ErrorKind::UnexpectedEof {
                if !quiet {
                    info!("Client disconnected");
                }
                break;
            }
            error!("Failed to read message length: {}", e);
            break;
        }

        let msg_len = u32::from_be_bytes(len_buf) as usize;

        // Limit message size to prevent allocation attacks
        if msg_len > 1024 * 1024 {
            error!("Message too large: {} bytes", msg_len);
            break;
        }

        // Read message
        let mut msg_buf = vec![0u8; msg_len];
        if let Err(e) = socket.read_exact(&mut msg_buf).await {
            error!("Failed to read message: {}", e);
            break;
        }

        // Parse request
        let request: Request = match serde_json::from_slice(&msg_buf) {
            Ok(r) => r,
            Err(e) => {
                error!("Failed to parse request: {}", e);
                let response = Response::Error {
                    message: format!("Invalid request: {}", e),
                };
                let _ = send_response(&mut socket, &response).await;
                continue;
            }
        };

        // Handle request
        let response = {
            let mut state_guard = state.lock().unwrap();
            handle_request(&mut state_guard, request)
        };

        // Send response
        if let Err(e) = send_response(&mut socket, &response).await {
            error!("Failed to send response: {}", e);
            break;
        }
    }
}

/// Send a response message with length prefix
async fn send_response(
    socket: &mut tokio::net::TcpStream,
    response: &Response,
) -> std::io::Result<()> {
    let response_json = serde_json::to_string(response).unwrap();
    let response_bytes = response_json.as_bytes();

    let len = response_bytes.len() as u32;
    socket.write_all(&len.to_be_bytes()).await?;
    socket.write_all(response_bytes).await?;

    Ok(())
}

/// Handle a single request
fn handle_request(state: &mut MachineState, request: Request) -> Response {
    match request {
        Request::ReadMemory { start, end } => {
            let mut data = Vec::new();
            let mut addr = start;

            while addr <= end {
                let byte = read_memory_byte(state, addr);
                data.push(byte);
                addr = addr.wrapping_add(1);
            }

            let hexdump = format_hexdump(&data, start);
            Response::MemoryData { data: hexdump }
        }
        Request::Step => {
            let result = state.machine.tick();
            match result {
                ExecuteResult::Continue => Response::StepResult { stopped: false },
                ExecuteResult::Stop(code) => {
                    info!("Machine stopped with code: {}", code);
                    Response::StepResult { stopped: true }
                }
                ExecuteResult::Halt => {
                    info!("Machine halted");
                    Response::StepResult { stopped: true }
                }
                ExecuteResult::ShouldReset => {
                    state.machine.reset();
                    warn!("Machine reset during step");
                    Response::StepResult { stopped: false }
                }
            }
        }
        Request::Reset => {
            state.machine.reset();
            Response::ResetDone
        }
        Request::GetStatus => {
            let status = get_machine_status(state);
            Response::Status { status }
        }
        Request::GetCpuRegisters => {
            let registers = get_cpu_registers(state);
            Response::CpuRegisters { registers }
        }
        Request::GetApuStatus => {
            let status = get_apu_status(state);
            Response::ApuStatus { status }
        }
        Request::GetPpuStatus => {
            let status = get_ppu_status(state);
            Response::PpuStatus { status }
        }
        Request::ReadOam => {
            let data = get_oam_data(state);
            Response::OamData { data }
        }
        Request::ReadNametable { index } => {
            let data = get_nametable_data(state, index);
            Response::NametableData { data }
        }
    }
}

/// Read a single byte from memory
fn read_memory_byte(state: &MachineState, addr: u16) -> u8 {
    match &state.machine {
        MachineWrapper::Bin(m) => m.mcu().peek(addr),
        MachineWrapper::INes(m) => m.mcu().peek(addr),
        MachineWrapper::PngFrameMatch(m) => m.mcu().peek(addr),
    }
}

/// Get current machine status
fn get_machine_status(state: &mut MachineState) -> nes_mcp_protocol::MachineStatus {
    match &mut state.machine {
        MachineWrapper::Bin(m) => {
            let cpu = m.cpu_mut();
            nes_mcp_protocol::MachineStatus {
                pc: cpu.pc,
                a: cpu.a,
                x: cpu.x,
                y: cpu.y,
                p: cpu.status,
                sp: cpu.sp,
                cycles: cpu.total_cycles() as u64,
            }
        }
        // INes and PngFrameMatch don't expose CPU state through public API
        MachineWrapper::INes(_) | MachineWrapper::PngFrameMatch(_) => {
            nes_mcp_protocol::MachineStatus::default()
        }
    }
}

/// Get CPU register state with decoded flags
fn get_cpu_registers(state: &mut MachineState) -> nes_mcp_protocol::CpuRegisters {
    match &mut state.machine {
        MachineWrapper::Bin(m) => {
            let cpu = m.cpu_mut();
            let status = cpu.status;
            nes_mcp_protocol::CpuRegisters {
                pc: cpu.pc,
                a: cpu.a,
                x: cpu.x,
                y: cpu.y,
                sp: cpu.sp,
                status,
                cycles: cpu.total_cycles() as u64,
                flag_n: (status & 0x80) != 0,
                flag_v: (status & 0x40) != 0,
                flag_d: (status & 0x08) != 0,
                flag_i: (status & 0x04) != 0,
                flag_z: (status & 0x02) != 0,
                flag_c: (status & 0x01) != 0,
            }
        }
        MachineWrapper::INes(_) | MachineWrapper::PngFrameMatch(_) => {
            nes_mcp_protocol::CpuRegisters {
                pc: 0,
                a: 0,
                x: 0,
                y: 0,
                sp: 0,
                status: 0,
                cycles: 0,
                flag_n: false,
                flag_v: false,
                flag_d: false,
                flag_i: false,
                flag_z: false,
                flag_c: false,
            }
        }
    }
}

/// Get APU status
fn get_apu_status(state: &mut MachineState) -> nes_mcp_protocol::ApuStatus {
    match &state.machine {
        MachineWrapper::Bin(_) => {
            // Bin machines don't have APU
            nes_mcp_protocol::ApuStatus {
                pulse1_enabled: false,
                pulse2_enabled: false,
                triangle_enabled: false,
                noise_enabled: false,
                dmc_enabled: false,
                frame_irq_pending: false,
                dmc_irq_pending: false,
            }
        }
        MachineWrapper::INes(m) => {
            let apu_status = m.mcu().dump_apu_state();
            nes_mcp_protocol::ApuStatus {
                pulse1_enabled: apu_status.pulse1_enabled,
                pulse2_enabled: apu_status.pulse2_enabled,
                triangle_enabled: apu_status.triangle_enabled,
                noise_enabled: apu_status.noise_enabled,
                dmc_enabled: apu_status.dmc_enabled,
                frame_irq_pending: apu_status.frame_irq_pending,
                dmc_irq_pending: apu_status.dmc_irq_pending,
            }
        }
        MachineWrapper::PngFrameMatch(m) => {
            let apu_status = m.mcu().dump_apu_state();
            nes_mcp_protocol::ApuStatus {
                pulse1_enabled: apu_status.pulse1_enabled,
                pulse2_enabled: apu_status.pulse2_enabled,
                triangle_enabled: apu_status.triangle_enabled,
                noise_enabled: apu_status.noise_enabled,
                dmc_enabled: apu_status.dmc_enabled,
                frame_irq_pending: apu_status.frame_irq_pending,
                dmc_irq_pending: apu_status.dmc_irq_pending,
            }
        }
    }
}

/// Get PPU state dump
fn get_ppu_status(state: &MachineState) -> String {
    match &state.machine {
        MachineWrapper::Bin(_) => {
            "# PPU State\nNot available (Bin machine has no PPU)\n".to_string()
        }
        MachineWrapper::INes(m) => m.mcu().dump_ppu_state(),
        MachineWrapper::PngFrameMatch(m) => m.mcu().dump_ppu_state(),
    }
}

/// Get OAM data as hexdump
fn get_oam_data(state: &MachineState) -> String {
    match &state.machine {
        MachineWrapper::Bin(_) => "OAM not available (Bin machine has no PPU)\n".to_string(),
        MachineWrapper::INes(m) => {
            let oam = m.mcu().get_oam_data();
            format_hexdump(&oam, 0)
        }
        MachineWrapper::PngFrameMatch(m) => {
            let oam = m.mcu().get_oam_data();
            format_hexdump(&oam, 0)
        }
    }
}

/// Get nametable data as hexdump
fn get_nametable_data(state: &MachineState, index: u8) -> String {
    match &state.machine {
        MachineWrapper::Bin(_) => "Nametable not available (Bin machine has no PPU)\n".to_string(),
        MachineWrapper::INes(m) => {
            let nametable = m.mcu().get_nametable_data(index);
            format_hexdump(&nametable, 0x2000 + (index as u16 * 0x400))
        }
        MachineWrapper::PngFrameMatch(m) => {
            let nametable = m.mcu().get_nametable_data(index);
            format_hexdump(&nametable, 0x2000 + (index as u16 * 0x400))
        }
    }
}

/// Run the TCP server
pub fn run_tcp_server(
    rom_path: PathBuf,
    quiet: bool,
    start_pc: Option<String>,
    max_instructions: u64,
) -> ! {
    // Parse start_pc if provided
    let start_pc = match start_pc {
        Some(s) => {
            let parsed = if s.starts_with("0x") || s.starts_with("0X") {
                u16::from_str_radix(&s[2..], 16)
            } else {
                s.parse::<u16>()
            };
            Some(parsed.expect("invalid --start-pc value"))
        }
        None => None,
    };

    // Load ROM and create machine
    let image = load_image(rom_path).unwrap();
    let machine = image.create_machine(quiet, start_pc, max_instructions);

    let state = Arc::new(Mutex::new(MachineState { machine, quiet }));

    // Create tokio runtime (single-threaded to avoid Send requirements)
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    rt.block_on(async {
        let addr = format!("{}:{}", TCP_HOST, TCP_PORT);
        let listener = match TcpListener::bind(&addr).await {
            Ok(l) => {
                info!("TCP server listening on {}", addr);
                l
            }
            Err(e) => {
                error!("Failed to bind to {}: {}", addr, e);
                std::process::exit(1);
            }
        };

        loop {
            match listener.accept().await {
                Ok((socket, _addr)) => {
                    // Handle client sequentially (single MCP server expected)
                    handle_client(socket, state.clone()).await;
                }
                Err(e) => {
                    error!("Failed to accept connection: {}", e);
                }
            }
        }
    });

    // The runtime above never returns, but we need to satisfy the ! return type
    std::process::exit(1)
}
