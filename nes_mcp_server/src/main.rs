use anyhow::Result;
use nes_mcp_protocol::{Request, Response};
use rmcp::{
    ServerHandler, ServiceExt,
    handler::server::{router::tool::ToolRouter, tool::Parameters},
    model::{PaginatedRequestParam, ReadResourceRequestParam, ServerCapabilities, ServerInfo},
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
use tokio::sync::Mutex;
use tracing::{error, info};

#[derive(Deserialize, schemars::JsonSchema)]
struct StartParams {
    rom_path: String,
}

const TCP_PORT: u16 = 28800;
const TCP_HOST: &str = "127.0.0.1";

pub struct NesMcpServer {
    tool_router: ToolRouter<NesMcpServer>,
    child_process: Arc<Mutex<Option<tokio::process::Child>>>,
    rom_path: Arc<Mutex<Option<String>>>,
}

#[tool_router]
impl NesMcpServer {
    fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
            child_process: Arc::new(Mutex::new(None)),
            rom_path: Arc::new(Mutex::new(None)),
        }
    }

    /// Connect to TCP server and send request
    async fn send_tcp_request(&self, request: &Request) -> Result<Response> {
        // Retry connection a few times with delay
        let mut stream = None;
        for attempt in 0..5 {
            match tokio::net::TcpStream::connect((TCP_HOST, TCP_PORT)).await {
                Ok(s) => {
                    stream = Some(s);
                    break;
                }
                Err(_e) if attempt < 4 => {
                    info!("Connection attempt {} failed, retrying...", attempt + 1);
                    tokio::time::sleep(Duration::from_millis(200)).await;
                }
                Err(e) => return Err(e.into()),
            }
        }

        let mut stream =
            stream.ok_or_else(|| anyhow::anyhow!("Failed to connect to TCP server"))?;

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
        let mut child_guard = self.child_process.lock().await;
        if let Some(mut child) = child_guard.take() {
            info!("Killing existing NES process before starting new one");
            let _ = child.kill().await;
            let _ = child.wait().await;
        }
        drop(child_guard);

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

        *self.child_process.lock().await = Some(child);

        // Wait for TCP server to be ready
        for attempt in 0..10 {
            tokio::time::sleep(Duration::from_millis(300)).await;
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
        let mut child_guard = self.child_process.lock().await;
        if let Some(mut child) = child_guard.take() {
            info!("Killing existing NES process");
            let _ = child.kill().await;
            let _ = child.wait().await;
        }
        drop(child_guard);

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

        *self.child_process.lock().await = Some(child);

        // Wait for TCP server to be ready
        for attempt in 0..10 {
            tokio::time::sleep(Duration::from_millis(300)).await;
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
        let raw = rmcp::model::RawResource {
            uri: "nes://memory/".to_string(),
            name: "NES Memory".to_string(),
            description: Some("Access NES emulator memory".to_string()),
            mime_type: Some("text/plain".to_string()),
            size: None,
        };
        Ok(rmcp::model::ListResourcesResult {
            resources: vec![rmcp::model::Resource {
                raw,
                annotations: None,
            }],
            next_cursor: None,
        })
    }

    async fn read_resource(
        &self,
        request: rmcp::model::ReadResourceRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> Result<rmcp::model::ReadResourceResult, rmcp::model::ErrorData> {
        let uri = &request.uri;

        if !uri.starts_with("nes://memory/") {
            return Err(rmcp::model::ErrorData::invalid_params(
                "Unknown resource URI",
                Some(serde_json::json!(uri.clone())),
            ));
        }

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
                    uri: uri.clone(),
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

    let transport = (tokio::io::stdin(), tokio::io::stdout());
    let service = NesMcpServer::new().serve(transport).await?;
    service.waiting().await?;

    Ok(())
}
