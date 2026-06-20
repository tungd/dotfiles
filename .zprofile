# Login/session environment for zsh.
# Interactive shell setup lives in ~/.zshrc.
# export LANG=en_US.UTF-8
export CLICOLOR=1
export ENV=development
export LOCAL=$HOME/.local
export PGHOST=127.0.0.1
export PGPASS=postgres
export PGPASSWORD=postgres
export PGUSER=postgres
export PATH=$HOME/.local/sbin:$HOME/.local/bin:$PATH
export PATH=$HOME/Projects/dotfiles/bin:$PATH
export PNPM_HOME="/Users/tung/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
export PATH="$HOME/.claude/local:$PATH"
# Added by Antigravity CLI installer
export PATH="/Users/tung/.local/bin:$PATH"

##
# Your previous /Users/tung/.zprofile file was backed up as /Users/tung/.zprofile.macports-saved_2026-06-07_at_10:51:45
##

# MacPorts Installer addition on 2026-06-07_at_10:51:45: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

path=(/opt/local/bin /opt/local/sbin $path)
typeset -U path PATH
