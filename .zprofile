# Login/session environment for zsh.
# Interactive shell setup lives in ~/.zshrc.
# export LANG=en_US.UTF-8
export CLICOLOR=1
export ENV=local
export LOCAL=$HOME/.local
export PGHOST=127.0.0.1
export PGPASS=postgres
export PGPASSWORD=postgres
export PGUSER=postgres
export PATH=$HOME/.local/sbin:$HOME/.local/bin:$PATH
export PATH=$HOME/Projects/dotfiles/bin:$PATH
export PNPM_HOME="/Users/tung/Library/pnpm"
export PATH="$PNPM_HOME/bin:$PATH"
export PATH="$HOME/.claude/local:$PATH"

# MacPorts Installer addition
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

path=(/opt/local/bin /opt/local/sbin $path)
typeset -U path PATH
