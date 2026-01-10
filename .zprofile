stty -ixon

fpath=($HOME/Projects/dotfiles/zsh_functions $fpath)

# export LANG=en_US.UTF-8
export CLICOLOR=1

autoload -Uz add-zsh-hook

autoload -Uz compinit
compinit

autoload -Uz colors
colors

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

autoload promptinit
promptinit
prompt tungd

export CDPATH=.:$HOME:~/Projects

export WORDCHARS='*?[]~&;!$%^<>'

export HISTFILE=$HOME/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000

# http://zsh.sourceforge.net/Doc/Release/Options.html
setopt auto_cd
setopt auto_pushd

setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups
setopt hist_reduce_blanks

setopt prompt_subst
setopt multios

zle -N newtab

zmodload -i zsh/complist
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' insert-tab pending

zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:*:*:*' menu select

bindkey -e
bindkey '^W' backward-kill-word
bindkey '\M\b' backward-kill-word

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# if [ "$TERM_PROGRAM" = "Apple_Terminal" ] || [ "$TERM_PROGRAM" = "iTerm.app" ]; then
#   update_terminal_cwd () {
#     local PWD_URL="file://$HOST${PWD// /%20}"
#     printf '\e]7;%s\a' "$PWD_URL"
#   }
#   update_terminal_cwd
#   add-zsh-hook chpwd update_terminal_cwd
# fi

if [[ $EMACS = t ]]; then
  unsetopt zle
  unset zle_bracketed_paste
fi

export ENV=development
export LOCAL=$HOME/.local

export PGHOST=127.0.0.1
export PGPASS=postgres
export PGPASSWORD=postgres
export PGUSER=postgres

export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export PATH=$HOME/.local/sbin:$HOME/.local/bin:$PATH
export PATH=$HOME/Projects/dotfiles/bin:$PATH
export PATH=$HOME/.opam/default/bin/:$PATH

[[ -e /opt/local/etc/profile.d/z.sh ]] && . /opt/local/etc/profile.d/z.sh
[[ $+commands[direnv] ]] && eval "$(direnv hook zsh)"
# [[ $+commands[thrash] ]] && alias rm=trash

alias g=git
alias e="emacsclient -n"
alias diff="diff --color -u"
alias imgcat="wezterm imgcat"

notify() {
  osascript -e "display notification \"$2\" with title \"$1\""
}

fixmod() {
    find $1 -type f -exec chmod 644 {} \;
    find $1 -type d -exec chmod 755 {} \;
}

myip() {
    dig +short txt ch whoami.cloudflare @1.0.0.1;
}

export PNPM_HOME="/Users/tung/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
export PATH="$HOME/.claude/local:$PATH"
export PATH="/Applications/WezTerm.app/Contents/MacOS/:$PATH"

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

preexec() {
  # Store the command being executed
  WEZTERM_CURRENT_COMMAND="$1"
  printf "\e]1337;SetUserVar=WEZTERM_COMMAND_STATUS=running\007"
}

precmd() {
  # Command finished - send notification trigger
  if [[ -n "$WEZTERM_CURRENT_COMMAND" ]]; then
    printf "\e]1337;SetUserVar=WEZTERM_COMMAND_STATUS=finished:%s\007" "$WEZTERM_CURRENT_COMMAND"
    unset WEZTERM_CURRENT_COMMAND
  fi
}
