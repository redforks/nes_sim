use anyhow::Result;
use nes_mcp_protocol::{Request, Response};
use rmcp::{
    ServerHandler, ServiceExt,
    handler::server::{router::tool::ToolRouter, tool::Parameters},
    model::{PaginatedRequestParam, ServerCapabilities, ServerInfo},
    schemars,
    service::{RequestContext, RoleServer},
    tool, tool_handler, tool_router,
};
use serde::Deserialize;
use std::future::Future;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::Command;
use tokio::signal::unix::{SignalKind, signal};
use tokio::sync::Mutex;
use tracing::{error, info, warn};

#[derive(Deserialize, schemars::JsonSchema)]
struct StartParams {
    rom_path: String,
}

#[derive(Deserialize, schemars::JsonSchema)]
struct TickParams {
    n_cycles: u64,
}

#[derive(Deserialize, schemars::JsonSchema)]
struct ForwardToVblankParams {}

#[derive(Deserialize, schemars::JsonSchema)]
#[allow(dead_code)]
struct NametableParams {
    index: Option<u8>,
}

const TCP_PORT: u16 = 28800;
const TCP_HOST: &str = "127.0.0.1";

const TCP_CONNECT_RETRY_ATTEMPTS: u8 = 5;
const TCP_CONNECT_RETRY_DELAY_MS: u64 = 200;
const TCP_SERVER_READY_ATTEMPTS: u8 = 10;
const TCP_SERVER_READY_DELAY_MS: u64 = 300;

pub struct NesMcpServer {
    tool_router: ToolRouter<NesMcpServer>,
    child_process: Arc<Mutex<Option<tokio::process::Child>>>,
    child_pid: Arc<Mutex<Option<u32>>>,
    rom_path: Arc<Mutex<Option<String>>>,
}

#[tool_router]
impl NesMcpServer {
    fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
            child_process: Arc::new(Mutex::new(None)),
            child_pid: Arc::new(Mutex::new(None)),
            rom_path: Arc::new(Mutex::new(None)),
        }
    }

    /// Kill the child process if it's running
    async fn kill_child_process(&self) {
        let mut child_guard = self.child_process.lock().await;
        if let Some(mut child) = child_guard.take() {
            let pid = child.id();
            info!("Killing NES emulator process (PID: {:?})", pid);

            // Try SIGTERM first for graceful shutdown
            if let Err(e) = child.kill().await {
                warn!("Failed to kill child process: {}", e);
            } else {
                // Wait for the process to actually exit
                match child.wait().await {
                    Ok(status) => {
                        info!("Child process exited with status: {}", status);
                    }
                    Err(e) => {
                        warn!("Failed to wait for child process: {}", e);
                    }
                }
            }

            *self.child_pid.lock().await = None;
        }
        drop(child_guard);
    }

    /// Connect to TCP server with retry logic
    async fn connect_tcp_with_retry(
        &self,
        max_attempts: u8,
        delay: Duration,
    ) -> Result<tokio::net::TcpStream> {
        for attempt in 0..max_attempts {
            match tokio::net::TcpStream::connect((TCP_HOST, TCP_PORT)).await {
                Ok(s) => return Ok(s),
                Err(_) if attempt < max_attempts - 1 => {
                    info!("Connection attempt {} failed, retrying...", attempt + 1);
                    tokio::time::sleep(delay).await;
                }
                Err(e) => return Err(e.into()),
            }
        }
        Err(anyhow::anyhow!(
            "Failed to connect to TCP server after {} attempts",
            max_attempts
        ))
    }

    /// Connect to TCP server and send request
    async fn send_tcp_request(&self, request: &Request) -> Result<Response> {
        let mut stream = self
            .connect_tcp_with_retry(
                TCP_CONNECT_RETRY_ATTEMPTS,
                Duration::from_millis(TCP_CONNECT_RETRY_DELAY_MS),
            )
            .await?;

        // Serialize request
        let request_json = serde_json::to_string(request)?;
        let request_bytes = request_json.as_bytes();

        // Send length prefix (4 bytes big-endian)
        let len = request_bytes.len() as u32;
        stream.write_all(&len.to_be_bytes()).await?;
        stream.write_all(request_bytes).await?;

        // Read response length
        let mut len_buf = [0u8; 4];
        stream.read_exact(&mut len_buf).await?;
        let response_len = u32::from_be_bytes(len_buf) as usize;

        // Read response
        let mut response_buf = vec![0u8; response_len];
        stream.read_exact(&mut response_buf).await?;

        let response: Response = serde_json::from_slice(&response_buf)?;
        Ok(response)
    }

    #[tool(description = "Start the NES emulator with the specified ROM/image file path.")]
    async fn start(&self, Parameters(StartParams { rom_path }): Parameters<StartParams>) -> String {
        // Validate the ROM path exists
        if !std::path::Path::new(&rom_path).exists() {
            return format!("Error: ROM file not found: {}", rom_path);
        }

        // Store the ROM path
        *self.rom_path.lock().await = Some(rom_path.clone());

        // Kill any existing process first
        self.kill_child_process().await;

        // Start new process with ROM
        info!("Starting NES CPU test with ROM: {}", rom_path);

        let child = match Command::new("cargo")
            .args([
                "run",
                "-p",
                "nes_cpu_test",
                "--features",
                "tcp-server",
                "--",
                "--tcp-server",
                "-f",
                &rom_path,
            ])
            .stdout(Stdio::null())
            .stderr(Stdio::inherit())
            .spawn()
        {
            Ok(c) => c,
            Err(e) => {
                error!("Failed to start nes_cpu_test: {}", e);
                return format!("Error: Failed to start nes_cpu_test: {}", e);
            }
        };

        let pid = child.id();
        *self.child_process.lock().await = Some(child);
        *self.child_pid.lock().await = pid;
        info!("Started NES CPU test process with PID: {:?}", pid);

        // Wait for TCP server to be ready
        for attempt in 0..TCP_SERVER_READY_ATTEMPTS {
            tokio::time::sleep(Duration::from_millis(TCP_SERVER_READY_DELAY_MS)).await;
            if tokio::net::TcpStream::connect((TCP_HOST, TCP_PORT))
                .await
                .is_ok()
            {
                info!("NES TCP server is ready");
                return format!("NES emulator started successfully with ROM: {}", rom_path);
            }
            info!("Waiting for TCP server... attempt {}", attempt + 1);
        }

        error!("TCP server did not become ready in time");
        *self.child_process.lock().await = None;
        "Error: TCP server did not become ready in time".to_string()
    }

    #[tool(
        description = "Restart the NES emulator. Kills any existing instance and starts a new one with the previously configured ROM."
    )]
    async fn restart_nes(&self) -> String {
        // Check if ROM path is set
        let rom_path = {
            let path_guard = self.rom_path.lock().await;
            path_guard.as_ref().cloned()
        };

        let rom_path = match rom_path {
            Some(path) => path,
            None => {
                return "Error: No ROM file configured. Please call 'start' tool first with a ROM file path.".to_string();
            }
        };

        // Kill existing process if any
        self.kill_child_process().await;

        // Start new process with ROM
        info!("Restarting NES CPU test with ROM: {}", rom_path);

        let child = match Command::new("cargo")
            .args([
                "run",
                "-p",
                "nes_cpu_test",
                "--features",
                "tcp-server",
                "--",
                "--tcp-server",
                "-f",
                &rom_path,
            ])
            .stdout(Stdio::null())
            .stderr(Stdio::inherit())
            .spawn()
        {
            Ok(c) => c,
            Err(e) => {
                error!("Failed to start nes_cpu_test: {}", e);
                return format!("Error: Failed to start nes_cpu_test: {}", e);
            }
        };

        let pid = child.id();
        *self.child_process.lock().await = Some(child);
        *self.child_pid.lock().await = pid;
        info!("Restarted NES CPU test process with PID: {:?}", pid);

        // Wait for TCP server to be ready
        for attempt in 0..TCP_SERVER_READY_ATTEMPTS {
            tokio::time::sleep(Duration::from_millis(TCP_SERVER_READY_DELAY_MS)).await;
            if tokio::net::TcpStream::connect((TCP_HOST, TCP_PORT))
                .await
                .is_ok()
            {
                info!("NES TCP server is ready");
                return "NES emulator restarted successfully".to_string();
            }
            info!("Waiting for TCP server... attempt {}", attempt + 1);
        }

        error!("TCP server did not become ready in time");
        *self.child_process.lock().await = None;
        "Error: TCP server did not become ready in time".to_string()
    }

    #[tool(description = "Execute N CPU instruction cycles on the NES. N must less than 1000000")]
    async fn tick(&self, Parameters(TickParams { n_cycles }): Parameters<TickParams>) -> String {
        const MAX_CYCLES: u64 = 1000000;

        if n_cycles > MAX_CYCLES {
            return format!(
                "Error: n_cycles exceeds maximum of {}. Requested: {}",
                MAX_CYCLES, n_cycles
            );
        }

        let mut cycles_executed = 0u64;
        let mut last_error = None;

        for _ in 0..n_cycles {
            match self.send_tcp_request(&Request::Step).await {
                Ok(Response::StepResult { stopped: true }) => {
                    return format!("Execution stopped after {} cycles", cycles_executed);
                }
                Ok(Response::StepResult { stopped: false }) => {
                    cycles_executed += 1;
                }
                Ok(Response::Error { message }) => {
                    last_error = Some(message);
                    break;
                }
                Err(e) => {
                    return format!("Error after {} cycles: {}", cycles_executed, e);
                }
                _ => {
                    last_error = Some("Unexpected response".to_string());
                    break;
                }
            }
        }

        if let Some(err) = last_error {
            format!("Error after {} cycles: {}", cycles_executed, err)
        } else {
            format!("Executed {} cycles", cycles_executed)
        }
    }

    #[tool(description = "Run the NES emulator until the next VBlank (scanline 241, dot 1).")]
    async fn forward_to_vblank(
        &self,
        Parameters(_params): Parameters<ForwardToVblankParams>,
    ) -> String {
        match self.send_tcp_request(&Request::ForwardToVblank).await {
            Ok(Response::ForwardToVblankResult { ticks }) => {
                format!("Execution stopped after {} ticks", ticks)
            }
            Ok(Response::Error { message }) => {
                format!("Error: {}", message)
            }
            Err(e) => {
                format!("Error: {}", e)
            }
            _ => "Error: Unexpected response".to_string(),
        }
    }
}

#[tool_handler]
impl ServerHandler for NesMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            capabilities: ServerCapabilities::builder()
                .enable_tools()
                .enable_resources()
                .build(),
            ..Default::default()
        }
    }

    async fn list_resources(
        &self,
        _request: Option<PaginatedRequestParam>,
        _context: RequestContext<RoleServer>,
    ) -> Result<rmcp::model::ListResourcesResult, rmcp::model::ErrorData> {
        let resources = vec![
            rmcp::model::Resource {
                raw: rmcp::model::RawResource {
                    uri: "nes://memory/".to_string(),
                    name: "NES Memory".to_string(),
                    description: Some(
                        "Access NES emulator memory. Use ?start=0x0000&end=0xFFFF format (max 4KB range)".to_string(),
                    ),
                    mime_type: Some("text/plain".to_string()),
                    size: None,
                },
                annotations: None,
            },
            rmcp::model::Resource {
                raw: rmcp::model::RawResource {
                    uri: "nes://cpu/registers".to_string(),
                    name: "CPU Registers".to_string(),
                    description: Some("Current CPU register state".to_string()),
                    mime_type: Some("application/json".to_string()),
                    size: None,
                },
                annotations: None,
            },
            rmcp::model::Resource {
                raw: rmcp::model::RawResource {
                    uri: "nes://apu/status".to_string(),
                    name: "APU Status".to_string(),
                    description: Some("Current APU status".to_string()),
                    mime_type: Some("application/json".to_string()),
                    size: None,
                },
                annotations: None,
            },
            rmcp::model::Resource {
                raw: rmcp::model::RawResource {
                    uri: "nes://ppu/status".to_string(),
                    name: "PPU Status".to_string(),
                    description: Some("PPU state dump".to_string()),
                    mime_type: Some("text/plain".to_string()),
                    size: None,
                },
                annotations: None,
            },
            rmcp::model::Resource {
                raw: rmcp::model::RawResource {
                    uri: "nes://ppu/oam".to_string(),
                    name: "PPU OAM".to_string(),
                    description: Some("Object Attribute Memory (sprite data)".to_string()),
                    mime_type: Some("text/plain".to_string()),
                    size: None,
                },
                annotations: None,
            },
            rmcp::model::Resource {
                raw: rmcp::model::RawResource {
                    uri: "nes://ppu/nametable".to_string(),
                    name: "PPU Nametable".to_string(),
                    description: Some(
                        "Nametable memory (use ?index=0-3 for specific table)".to_string(),
                    ),
                    mime_type: Some("text/plain".to_string()),
                    size: None,
                },
                annotations: None,
            },
        ];
        Ok(rmcp::model::ListResourcesResult {
            resources,
            next_cursor: None,
        })
    }

    async fn read_resource(
        &self,
        request: rmcp::model::ReadResourceRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        let uri = &request.uri;

        // Route to appropriate handler based on URI
        if uri.starts_with("nes://memory/") {
            return self.read_memory_resource(uri).await;
        } else if uri == "nes://cpu/registers" {
            return self.read_cpu_registers_resource(uri).await;
        } else if uri == "nes://apu/status" {
            return self.read_apu_status_resource(uri).await;
        } else if uri == "nes://ppu/status" {
            return self.read_ppu_status_resource(uri).await;
        } else if uri == "nes://ppu/oam" {
            return self.read_oam_resource(uri).await;
        } else if uri.starts_with("nes://ppu/nametable") {
            return self.read_nametable_resource(uri).await;
        }

        Err(rmcp::model::ErrorData::invalid_params(
            "Unknown resource URI",
            Some(serde_json::json!(uri.clone())),
        ))
    }
}

// Helper methods for resource reading
impl NesMcpServer {
    /// Read memory resource (existing functionality)
    async fn read_memory_resource(
        &self,
        uri: &str,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        // Parse query parameters
        let url = match url::Url::parse(uri) {
            Ok(u) => u,
            Err(e) => {
                return Err(rmcp::model::ErrorData::invalid_params(
                    "Invalid URI",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        let query_pairs = url.query.map(|q| {
            q.split('&')
                .filter_map(|s| {
                    let mut parts = s.splitn(2, '=');
                    Some((parts.next()?.to_string(), parts.next()?.to_string()))
                })
                .collect::<Vec<_>>()
        });

        let start_str = query_pairs
            .as_ref()
            .and_then(|pairs| {
                pairs
                    .iter()
                    .find(|(k, _)| *k == "start")
                    .map(|(_, v)| v.to_string())
            })
            .unwrap_or_else(|| "0x0000".to_string());

        let end_str = query_pairs
            .as_ref()
            .and_then(|pairs| {
                pairs
                    .iter()
                    .find(|(k, _)| *k == "end")
                    .map(|(_, v)| v.to_string())
            })
            .unwrap_or_else(|| "0xFFFF".to_string());

        let parse_hex = |s: &str| -> Result<u16, String> {
            if s.starts_with("0x") || s.starts_with("0X") {
                u16::from_str_radix(&s[2..], 16).map_err(|e| e.to_string())
            } else {
                s.parse::<u16>().map_err(|e| e.to_string())
            }
        };

        let start = parse_hex(&start_str).map_err(|e| {
            rmcp::model::ErrorData::invalid_params(
                "Invalid start address",
                Some(serde_json::json!(e)),
            )
        })?;
        let end = parse_hex(&end_str).map_err(|e| {
            rmcp::model::ErrorData::invalid_params(
                "Invalid end address",
                Some(serde_json::json!(e)),
            )
        })?;

        if start > end {
            return Err(rmcp::model::ErrorData::invalid_params(
                "Invalid address range",
                Some(serde_json::json!("start > end")),
            ));
        }

        if (end as usize) - (start as usize) > 4096 {
            return Err(rmcp::model::ErrorData::invalid_params(
                "Range too large",
                Some(serde_json::json!("max 4KB")),
            ));
        }

        // Query TCP server for memory data
        let response = match self
            .send_tcp_request(&Request::ReadMemory { start, end })
            .await
        {
            Ok(r) => r,
            Err(e) => {
                return Err(rmcp::model::ErrorData::internal_error(
                    "Failed to read memory",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        match response {
            Response::MemoryData { data } => Ok(rmcp::model::ReadResourceResult {
                contents: vec![rmcp::model::ResourceContents::TextResourceContents {
                    uri: uri.to_string(),
                    mime_type: Some("text/plain".to_string()),
                    text: data,
                }],
            }),
            Response::Error { message } => Err(rmcp::model::ErrorData::internal_error(
                "Memory read failed",
                Some(serde_json::json!(message)),
            )),
            _ => Err(rmcp::model::ErrorData::internal_error(
                "Unexpected response",
                None,
            )),
        }
    }

    /// Read CPU registers resource
    async fn read_cpu_registers_resource(
        &self,
        uri: &str,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        let response = match self.send_tcp_request(&Request::GetCpuRegisters).await {
            Ok(r) => r,
            Err(e) => {
                return Err(rmcp::model::ErrorData::internal_error(
                    "Failed to read CPU registers",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        match response {
            Response::CpuRegisters { registers } => {
                let json = serde_json::to_string_pretty(&registers).map_err(|e| {
                    rmcp::model::ErrorData::internal_error(
                        "Failed to serialize registers",
                        Some(serde_json::json!(e.to_string())),
                    )
                })?;
                Ok(rmcp::model::ReadResourceResult {
                    contents: vec![rmcp::model::ResourceContents::TextResourceContents {
                        uri: uri.to_string(),
                        mime_type: Some("application/json".to_string()),
                        text: json,
                    }],
                })
            }
            Response::Error { message } => Err(rmcp::model::ErrorData::internal_error(
                "Failed to read CPU registers",
                Some(serde_json::json!(message)),
            )),
            _ => Err(rmcp::model::ErrorData::internal_error(
                "Unexpected response",
                None,
            )),
        }
    }

    /// Read APU status resource
    async fn read_apu_status_resource(
        &self,
        uri: &str,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        let response = match self.send_tcp_request(&Request::GetApuStatus).await {
            Ok(r) => r,
            Err(e) => {
                return Err(rmcp::model::ErrorData::internal_error(
                    "Failed to read APU status",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        match response {
            Response::ApuStatus { status } => {
                let json = serde_json::to_string_pretty(&status).map_err(|e| {
                    rmcp::model::ErrorData::internal_error(
                        "Failed to serialize APU status",
                        Some(serde_json::json!(e.to_string())),
                    )
                })?;
                Ok(rmcp::model::ReadResourceResult {
                    contents: vec![rmcp::model::ResourceContents::TextResourceContents {
                        uri: uri.to_string(),
                        mime_type: Some("application/json".to_string()),
                        text: json,
                    }],
                })
            }
            Response::Error { message } => Err(rmcp::model::ErrorData::internal_error(
                "Failed to read APU status",
                Some(serde_json::json!(message)),
            )),
            _ => Err(rmcp::model::ErrorData::internal_error(
                "Unexpected response",
                None,
            )),
        }
    }

    /// Read PPU status resource
    async fn read_ppu_status_resource(
        &self,
        uri: &str,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        let response = match self.send_tcp_request(&Request::GetPpuStatus).await {
            Ok(r) => r,
            Err(e) => {
                return Err(rmcp::model::ErrorData::internal_error(
                    "Failed to read PPU status",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        match response {
            Response::PpuStatus { status } => Ok(rmcp::model::ReadResourceResult {
                contents: vec![rmcp::model::ResourceContents::TextResourceContents {
                    uri: uri.to_string(),
                    mime_type: Some("text/plain".to_string()),
                    text: status,
                }],
            }),
            Response::Error { message } => Err(rmcp::model::ErrorData::internal_error(
                "Failed to read PPU status",
                Some(serde_json::json!(message)),
            )),
            _ => Err(rmcp::model::ErrorData::internal_error(
                "Unexpected response",
                None,
            )),
        }
    }

    /// Read OAM resource
    async fn read_oam_resource(
        &self,
        uri: &str,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        let response = match self.send_tcp_request(&Request::ReadOam).await {
            Ok(r) => r,
            Err(e) => {
                return Err(rmcp::model::ErrorData::internal_error(
                    "Failed to read OAM",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        match response {
            Response::OamData { data } => Ok(rmcp::model::ReadResourceResult {
                contents: vec![rmcp::model::ResourceContents::TextResourceContents {
                    uri: uri.to_string(),
                    mime_type: Some("text/plain".to_string()),
                    text: data,
                }],
            }),
            Response::Error { message } => Err(rmcp::model::ErrorData::internal_error(
                "Failed to read OAM",
                Some(serde_json::json!(message)),
            )),
            _ => Err(rmcp::model::ErrorData::internal_error(
                "Unexpected response",
                None,
            )),
        }
    }

    /// Read nametable resource
    async fn read_nametable_resource(
        &self,
        uri: &str,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        // Parse index from query parameter (default: 255 = all nametables)
        let url = match url::Url::parse(uri) {
            Ok(u) => u,
            Err(e) => {
                return Err(rmcp::model::ErrorData::invalid_params(
                    "Invalid URI",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        let query_pairs = url.query.map(|q| {
            q.split('&')
                .filter_map(|s| {
                    let mut parts = s.splitn(2, '=');
                    Some((parts.next()?.to_string(), parts.next()?.to_string()))
                })
                .collect::<Vec<_>>()
        });

        let index = query_pairs
            .as_ref()
            .and_then(|pairs| pairs.iter().find(|(k, _)| *k == "index"))
            .map(|(_, v)| v.parse::<u8>().ok())
            .flatten()
            .unwrap_or(255); // Default to all nametables

        let response = match self
            .send_tcp_request(&Request::ReadNametable { index })
            .await
        {
            Ok(r) => r,
            Err(e) => {
                return Err(rmcp::model::ErrorData::internal_error(
                    "Failed to read nametable",
                    Some(serde_json::json!(e.to_string())),
                ));
            }
        };

        match response {
            Response::NametableData { data } => Ok(rmcp::model::ReadResourceResult {
                contents: vec![rmcp::model::ResourceContents::TextResourceContents {
                    uri: uri.to_string(),
                    mime_type: Some("text/plain".to_string()),
                    text: data,
                }],
            }),
            Response::Error { message } => Err(rmcp::model::ErrorData::internal_error(
                "Failed to read nametable",
                Some(serde_json::json!(message)),
            )),
            _ => Err(rmcp::model::ErrorData::internal_error(
                "Unexpected response",
                None,
            )),
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::builder()
                .with_default_directive(tracing::Level::INFO.into())
                .from_env_lossy(),
        )
        .init();

    info!("Starting NES MCP server");

    // Create server with shared state for cleanup
    let child_process = Arc::new(Mutex::new(None));
    let child_pid = Arc::new(Mutex::new(None));
    let rom_path = Arc::new(Mutex::new(None));

    let server = NesMcpServer {
        tool_router: NesMcpServer::tool_router(),
        child_process: child_process.clone(),
        child_pid: child_pid.clone(),
        rom_path: rom_path.clone(),
    };

    // Set up signal handlers for graceful shutdown
    let mut sigterm = signal(SignalKind::terminate())?;
    let mut sigint = signal(SignalKind::interrupt())?;

    // Clone Arcs for the shutdown task
    let shutdown_child_process = child_process.clone();
    let shutdown_child_pid = child_pid.clone();

    // Spawn a task to handle signals
    let shutdown_task = tokio::spawn(async move {
        tokio::select! {
            _ = sigterm.recv() => {
                info!("Received SIGTERM signal, shutting down...");
            }
            _ = sigint.recv() => {
                info!("Received SIGINT signal, shutting down...");
            }
        }
        // Kill the child process before exiting
        cleanup_child_process(&shutdown_child_process, &shutdown_child_pid).await;
    });

    // Start the MCP server
    let transport = (tokio::io::stdin(), tokio::io::stdout());
    let service = server.serve(transport).await?;

    // Wait for either the service to complete or shutdown signal
    tokio::select! {
        result = service.waiting() => {
            result?;
        }
        _ = shutdown_task => {
            info!("Shutdown signal received");
        }
    }

    // Ensure child process is killed on exit
    cleanup_child_process(&child_process, &child_pid).await;
    info!("NES MCP server shutdown complete");

    Ok(())
}

/// Cleanup function to kill the child process
async fn cleanup_child_process(
    child_process: &Arc<Mutex<Option<tokio::process::Child>>>,
    child_pid: &Arc<Mutex<Option<u32>>>,
) {
    let mut child_guard = child_process.lock().await;
    if let Some(mut child) = child_guard.take() {
        let pid = child.id();
        info!("Killing NES emulator process (PID: {:?})", pid);

        // Try to kill the process gracefully
        if let Err(e) = child.kill().await {
            warn!("Failed to kill child process: {}", e);
        } else {
            // Wait for the process to actually exit
            match child.wait().await {
                Ok(status) => {
                    info!("Child process exited with status: {}", status);
                }
                Err(e) => {
                    warn!("Failed to wait for child process: {}", e);
                }
            }
        }

        *child_pid.lock().await = None;
    }
    drop(child_guard);
}
