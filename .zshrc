# Interactive zsh setup.
# Login/session environment lives in ~/.zprofile.

if [[ $TERM = dumb && -n $SSH_CONNECTION ]]; then
  return 0
fi

[[ -t 0 ]] && stty -ixon

if [[ ! -o login && -x /usr/libexec/path_helper ]]; then
  eval "$(/usr/libexec/path_helper -s)"
fi

# Core paths for non-login interactive shells such as tterm.
path=(
  /opt/local/bin
  /opt/local/sbin
  $HOME/.local/sbin
  $HOME/.local/bin
  $HOME/Projects/dotfiles/bin
  $HOME/.claude/local
  $path
)
typeset -U path PATH

[[ ! -r $HOME/.opam/opam-init/init.zsh ]] || source $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null

fpath=($HOME/Projects/dotfiles/zsh_functions $fpath)
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

[[ $+commands[direnv] ]] && eval "$(direnv hook zsh)"
# [[ $+commands[zoxide] ]] && eval "$(zoxide init zsh)"
. /opt/local/etc/profile.d/z.sh

alias g=git
alias e="emacsclient -n"
alias s="scv"
alias diff="diff --color -u"
alias tterm="tmux -L tterm-default-local attach -t tterm-default"

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

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/tung/.lmstudio/bin"
# End of LM Studio CLI section


# Added by Antigravity
export PATH="/Users/tung/.antigravity/antigravity/bin:$PATH"

# pnpm
export PNPM_HOME="/Users/tung/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME/bin:"*) ;;
  *) export PATH="$PNPM_HOME/bin:$PATH" ;;
esac
# pnpm end

export WASMTIME_HOME="$HOME/.wasmtime"

export PATH="$WASMTIME_HOME/bin:$PATH"

# NPM global bin (added by Qwen Code installer)
export PATH="$HOME/.npm-global/bin:$PATH"

# opencode
export PATH=/Users/tung/.opencode/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/tung/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/tung/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/tung/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/tung/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

# >>> grok installer >>>
export PATH="$HOME/.grok/bin:$PATH"
# <<< grok installer <<<

# Added by Antigravity IDE
export PATH="/Users/tung/.antigravity-ide/antigravity-ide/bin:$PATH"


# Added by Antigravity CLI installer
export PATH="/Users/tung/.local/bin:$PATH"

# Keep MacPorts ahead of installer paths added above.
path=(/opt/local/bin /opt/local/sbin $path)
typeset -U path PATH

# tterm current-directory tracking (OSC 7)
tterm_osc7_cwd() {
  printf '\e]7;file://%s%s\a' "${HOST:-localhost}" "$PWD"
}
add-zsh-hook precmd tterm_osc7_cwd
