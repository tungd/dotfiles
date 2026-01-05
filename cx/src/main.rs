mod cli;
mod config;
mod pty;
mod worktree;

use anyhow::{Context, Result};
use clap::Parser;

use cli::Cli;
use config::{Config, Profile};
use pty::run_proxy;
use worktree::WorktreeManager;

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Load config
    let config = Config::load().unwrap_or_else(|e| {
        eprintln!("Warning: Failed to load config: {}", e);
        Config::default()
    });

    // Resolve profile
    let profile = if let Some(name) = &cli.profile {
        config
            .get_profile(name)
            .cloned()
            .unwrap_or_else(|| {
                eprintln!("Warning: Profile '{}' not found, using defaults", name);
                Profile::default()
            })
    } else {
        Profile::default()
    };

    // Build environment variables from profile
    let env_vars = profile.env_vars();

    // Handle worktree
    let working_dir = if let Some(branch) = &cli.worktree {
        let manager = WorktreeManager::new().context("Failed to initialize worktree manager")?;
        let path = manager
            .ensure_worktree(branch)
            .with_context(|| format!("Failed to ensure worktree for branch '{}'", branch))?;
        eprintln!("Using worktree: {}", path.display());
        Some(path)
    } else {
        None
    };

    // Change to working directory if needed
    if let Some(ref dir) = working_dir {
        std::env::set_current_dir(dir)
            .with_context(|| format!("Failed to change to directory: {}", dir.display()))?;
    }

    // Build claude arguments
    let mut claude_args = Vec::new();

    // Add resume/continue flags
    if cli.resume {
        claude_args.push("-r".to_string());
    } else if cli.continue_session {
        claude_args.push("-c".to_string());
    }

    // Add any additional arguments
    claude_args.extend(cli.claude_args);

    // Determine VSync settings
    let vsync_fps = if cli.no_vsync {
        None
    } else {
        Some(cli.fps)
    };

    // Get claude path
    let claude_path = config.claude_path();

    // Verify claude exists
    if !claude_path.exists() {
        anyhow::bail!(
            "Claude CLI not found at: {}\nSet claude_path in ~/.config/cx/profiles.toml",
            claude_path.display()
        );
    }

    // Run the proxy
    let exit_code = run_proxy(&claude_path, &claude_args, &env_vars, vsync_fps).await?;

    std::process::exit(exit_code);
}
