stty -ixon

fpath=($HOME/Projects/dotfiles/zsh_functions $fpath)

export LANG=en_US.UTF-8
export CLICOLOR=1

autoload -Uz add-zsh-hook

autoload -Uz compinit
compinit

autoload -Uz colors
colors

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

if [ "$TERM_PROGRAM" = "Apple_Terminal" ] || [ "$TERM_PROGRAM" = "iTerm.app" ]; then
  update_terminal_cwd () {
    local PWD_URL="file://$HOST${PWD// /%20}"
    printf '\e]7;%s\a' "$PWD_URL"
  }
  update_terminal_cwd
  add-zsh-hook chpwd update_terminal_cwd
fi

if [[ $EMACS = t ]]; then
  unsetopt zle
  unset zle_bracketed_paste
fi

# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export ENV=development
export LOCAL=$HOME/.local

export PGHOST=127.0.0.1
export PGPASS=postgres
export PGPASSWORD=postgres
export PGUSER=postgres

export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export PATH=$HOME/.local/sbin:$HOME/.local/bin:$PATH
export PATH=$HOME/Library/Python/3.8/bin:$PATH
export PATH=$HOME/Projects/dotfiles/bin:$PATH
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export PATH=$HOME/Applications/Emacs.app/Contents/MacOS/bin:$PATH

which doctl > /dev/null && eval "$(doctl completion zsh)"
which direnv > /dev/null && eval "$(direnv hook zsh)"

which go > /dev/null && eval "$(go env)"
export PATH=$GOPATH/bin:$PATH

[[ -e $LOCAL/vendor/z.sh ]] && . $LOCAL/vendor/z.sh

which bat > /dev/null && alias cat=bat

which kubectl > /dev/null && source <(kubectl completion zsh)

alias g=git
alias e=emacsclient -n

notify() {
  osascript -e "display notification \"$2\" with title \"$1\""
}

export PATH="$HOME/.cargo/bin:$PATH"

# Setting PATH for Python 3.8
# The original version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.8/bin:${PATH}"
export PATH
