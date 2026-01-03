use anyhow::{Context, Result};
use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::io::{Read, Write};
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::io::AsyncWriteExt;
use tokio::sync::mpsc;

/// VSync buffer that batches output and flushes at a target frame rate
pub struct VSyncBuffer {
    buffer: Vec<u8>,
    last_flush: Instant,
    frame_duration: Duration,
}

impl VSyncBuffer {
    pub fn new(fps: u32) -> Self {
        Self {
            buffer: Vec::with_capacity(64 * 1024), // 64KB initial capacity
            last_flush: Instant::now(),
            frame_duration: Duration::from_micros(1_000_000 / fps as u64),
        }
    }

    pub fn push(&mut self, data: &[u8]) {
        self.buffer.extend_from_slice(data);
    }

    pub fn should_flush(&self) -> bool {
        self.last_flush.elapsed() >= self.frame_duration
    }

    pub fn flush(&mut self) -> Option<Vec<u8>> {
        if self.buffer.is_empty() {
            return None;
        }
        self.last_flush = Instant::now();
        Some(std::mem::take(&mut self.buffer))
    }

    pub fn force_flush(&mut self) -> Option<Vec<u8>> {
        if self.buffer.is_empty() {
            return None;
        }
        Some(std::mem::take(&mut self.buffer))
    }
}

/// Terminal guard that restores terminal mode on drop
struct TerminalGuard {
    was_raw: bool,
}

impl TerminalGuard {
    fn new() -> Result<Self> {
        crossterm::terminal::enable_raw_mode().context("Failed to enable raw mode")?;
        Ok(Self { was_raw: true })
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        if self.was_raw {
            let _ = crossterm::terminal::disable_raw_mode();
        }
    }
}

/// Run the PTY proxy
pub async fn run_proxy(
    claude_path: &Path,
    args: &[String],
    env_vars: &[(String, String)],
    vsync_fps: Option<u32>,
) -> Result<i32> {
    // Get current terminal size
    let (cols, rows) = crossterm::terminal::size().context("Failed to get terminal size")?;

    // Create PTY with matching size
    let pty_system = native_pty_system();
    let pty_pair = pty_system
        .openpty(PtySize {
            rows,
            cols,
            pixel_width: 0,
            pixel_height: 0,
        })
        .context("Failed to open PTY")?;

    // Build command with environment
    let mut cmd = CommandBuilder::new(claude_path);
    cmd.args(args);
    for (key, value) in env_vars {
        cmd.env(key, value);
    }

    // Spawn claude in the PTY
    let mut child = pty_pair
        .slave
        .spawn_command(cmd)
        .context("Failed to spawn claude")?;

    // Close slave in parent (important!)
    drop(pty_pair.slave);

    // Get read/write handles
    let mut pty_reader = pty_pair
        .master
        .try_clone_reader()
        .context("Failed to clone PTY reader")?;
    let pty_writer = pty_pair
        .master
        .take_writer()
        .context("Failed to take PTY writer")?;

    // For resize operations we need to keep master reference
    let pty_master = pty_pair.master;

    // Set up terminal in raw mode
    let _terminal_guard = TerminalGuard::new()?;

    // Create channels for communication between threads
    let (pty_tx, mut pty_rx) = mpsc::channel::<Vec<u8>>(100);
    let (stdin_tx, mut stdin_rx) = mpsc::channel::<Vec<u8>>(100);

    // Shutdown flag
    let shutdown = Arc::new(AtomicBool::new(false));
    let shutdown_clone = shutdown.clone();

    // Spawn thread to read from PTY (blocking I/O)
    let pty_read_handle = std::thread::spawn(move || {
        let mut buf = [0u8; 4096];
        loop {
            match pty_reader.read(&mut buf) {
                Ok(0) => break, // EOF
                Ok(n) => {
                    if pty_tx.blocking_send(buf[..n].to_vec()).is_err() {
                        break;
                    }
                }
                Err(e) => {
                    // Check if it's just EOF/closed
                    if e.kind() != std::io::ErrorKind::UnexpectedEof {
                        eprintln!("PTY read error: {}", e);
                    }
                    break;
                }
            }
        }
    });

    // Spawn thread to read from stdin (blocking I/O)
    let stdin_read_handle = std::thread::spawn(move || {
        let mut stdin = std::io::stdin();
        let mut buf = [0u8; 1024];
        while !shutdown_clone.load(Ordering::Relaxed) {
            match stdin.read(&mut buf) {
                Ok(0) => break, // EOF
                Ok(n) => {
                    if stdin_tx.blocking_send(buf[..n].to_vec()).is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
    });

    // Wrap pty_writer in Arc<Mutex> for shared access
    let pty_writer = Arc::new(std::sync::Mutex::new(pty_writer));
    let pty_writer_clone = pty_writer.clone();

    // Set up async stdout
    let mut stdout = tokio::io::stdout();

    // Create VSync buffer if enabled
    let mut vsync = vsync_fps.map(VSyncBuffer::new);

    // Frame interval for flush timer
    let flush_interval = vsync_fps
        .map(|fps| Duration::from_micros(1_000_000 / fps as u64))
        .unwrap_or(Duration::from_millis(16));

    let mut flush_timer = tokio::time::interval(flush_interval);
    flush_timer.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

    // Set up signal handlers
    #[cfg(unix)]
    let mut sigwinch = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::window_change())
        .context("Failed to set up SIGWINCH handler")?;

    #[cfg(unix)]
    let mut sigint = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::interrupt())
        .context("Failed to set up SIGINT handler")?;

    #[cfg(unix)]
    let mut sigterm = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
        .context("Failed to set up SIGTERM handler")?;

    // Main event loop
    loop {
        tokio::select! {
            // PTY output -> VSync buffer -> stdout
            Some(data) = pty_rx.recv() => {
                if let Some(ref mut buf) = vsync {
                    buf.push(&data);
                } else {
                    stdout.write_all(&data).await?;
                    stdout.flush().await?;
                }
            }

            // VSync flush timer
            _ = flush_timer.tick(), if vsync.is_some() => {
                if let Some(ref mut buf) = vsync {
                    if buf.should_flush() {
                        if let Some(data) = buf.flush() {
                            stdout.write_all(&data).await?;
                            stdout.flush().await?;
                        }
                    }
                }
            }

            // stdin -> PTY
            Some(data) = stdin_rx.recv() => {
                if let Ok(mut writer) = pty_writer_clone.lock() {
                    let _ = writer.write_all(&data);
                    let _ = writer.flush();
                }
            }

            // SIGWINCH -> resize PTY
            _ = sigwinch.recv() => {
                if let Ok((cols, rows)) = crossterm::terminal::size() {
                    let _ = pty_master.resize(PtySize {
                        rows,
                        cols,
                        pixel_width: 0,
                        pixel_height: 0,
                    });
                }
            }

            // SIGINT -> forward to child
            _ = sigint.recv() => {
                #[cfg(unix)]
                if let Some(pid) = child.process_id() {
                    unsafe {
                        libc::kill(pid as i32, libc::SIGINT);
                    }
                }
            }

            // SIGTERM -> forward to child and exit
            _ = sigterm.recv() => {
                #[cfg(unix)]
                if let Some(pid) = child.process_id() {
                    unsafe {
                        libc::kill(pid as i32, libc::SIGTERM);
                    }
                }
                break;
            }

            else => break,
        }
    }

    // Signal shutdown to stdin thread
    shutdown.store(true, Ordering::Relaxed);

    // Flush any remaining buffered output
    if let Some(ref mut buf) = vsync {
        if let Some(data) = buf.force_flush() {
            stdout.write_all(&data).await?;
            stdout.flush().await?;
        }
    }

    // Wait for child and get exit code
    let status = child.wait().context("Failed to wait for child process")?;

    // Clean up threads (they should exit when PTY closes)
    let _ = pty_read_handle.join();
    // stdin thread may block, don't wait for it
    drop(stdin_read_handle);

    Ok(status.exit_code() as i32)
}
