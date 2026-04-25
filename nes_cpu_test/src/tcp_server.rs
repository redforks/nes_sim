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
    info!("Client connected");

    loop {
        // Read message length
        let mut len_buf = [0u8; 4];
        if let Err(e) = socket.read_exact(&mut len_buf).await {
            if e.kind() == std::io::ErrorKind::UnexpectedEof {
                info!("Client disconnected");
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
                send_response(&mut socket, &response).await;
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
        MachineWrapper::INes(_) => {
            // For NesMachine, we can't directly access CPU state through the public API
            // Return a placeholder status for now
            nes_mcp_protocol::MachineStatus {
                pc: 0,
                a: 0,
                x: 0,
                y: 0,
                p: 0,
                sp: 0,
                cycles: 0,
            }
        }
        MachineWrapper::PngFrameMatch(_) => {
            // For PngFrameMatch, we can't directly access CPU state through the public API
            // Return a placeholder status for now
            nes_mcp_protocol::MachineStatus {
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
