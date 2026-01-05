use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "cx")]
#[command(about = "Claude Code wrapper with VSync buffering, profiles, and git worktree support")]
#[command(version)]
pub struct Cli {
    /// Profile to use (from ~/.config/cx/profiles.toml)
    #[arg(short, long)]
    pub profile: Option<String>,

    /// Target branch for git worktree (auto-creates if needed)
    #[arg(short, long)]
    pub worktree: Option<String>,

    /// Resume a previous session (passes -r to claude)
    #[arg(short, long, conflicts_with = "continue_session")]
    pub resume: bool,

    /// Continue most recent session (passes -c to claude)
    #[arg(short = 'c', long = "continue", conflicts_with = "resume")]
    pub continue_session: bool,

    /// VSync frame rate (30-60 fps)
    #[arg(long, default_value = "60", value_parser = clap::value_parser!(u32).range(30..=120))]
    pub fps: u32,

    /// Disable VSync buffering (raw passthrough)
    #[arg(long)]
    pub no_vsync: bool,

    /// Additional arguments passed to claude
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub claude_args: Vec<String>,
}
