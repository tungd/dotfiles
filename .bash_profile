export LANG=en_US.UTF-8
# export LC_ALL=$LANG
export LC_CTYPE=$LANG

# Colors
export CLICOLOR=1
export NC='\033[0m'
export WHITE='\033[1;37m'
export BLACK='\033[0;30m'
export BLUE='\033[0;34m'
export LIGHT_BLUE='\033[1;34m'
export GREEN='\033[0;32m'
export LIGHT_GREEN='\033[1;32m'
export CYAN='\033[0;36m'
export LIGHT_CYAN='\033[1;36m'
export RED='\033[0;31m'
export LIGHT_RED='\033[1;31m'
export PURPLE='\033[0;35m'
export LIGHT_PURPLE='\033[1;35m'
export YELLOW='\033[0;33m'
export LIGHT_YELLOW='\033[1;33m'
export GRAY='\033[1;30m'
export LIGHT_GRAY='\033[0;37m'
export END='\033[m'
export BOLD='\033[1m'


# History
shopt -s histappend
export HISTCONTROL=ignoredups
export HISTFILESIZE=65536
export HISTIGNORE="ls:[bf]g:exit:..:...:ll:lla"
export PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
alias h=history
hf() {
  grep "$@ $HOME/.bash_history"
}

function xtitle {
  unset PROMPT_COMMAND
  echo -ne "\033]0;$1\007"
}

# git prompt
function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  if [[ -n $(git status -s --ignore-submodules=dirty 2> /dev/null) ]]; then
    echo -ne "$RED+"
  fi
  echo -e "[${ref#refs/heads/}:$(git_prompt_short_sha)]$END"
}

function git_prompt_short_sha() {
  sha=$(git rev-parse --short HEAD 2> /dev/null) && echo "$sha"
}

export VIRTUAL_ENV_DISABLE_PROMPT=1
function env_prompt_info() {
  if [ ! -z "$VIRTUAL_ENV" ]; then
    echo -ne "$BLUE[pyvenv:$(basename $VIRTUAL_ENV)]$END"
  fi
}

function todo_prompt_info() {
  todo=$(ag --nogroup TODO | wc -l | tr -d ' \n') || return
  echo -ne "[todo:$todo]"
}

export PS1="\[\033[G\]
$RED\u$END@$YELLOW\H$END in $GREEN\w$END \$(git_prompt_info) \$(env_prompt_info)
› "

export ENV=development
export LOCAL=$HOME/.local
export PATH=$LOCAL/bin:$LOCAL/sbin:$PATH
export GEMOPTS='--no-rdoc --no-ri'
export GOPATH=$HOME/Projects/go
export PATH=$GOPATH/bin:$PATH
export PATH=$HOME/Library/Python/2.7/bin/:$PATH
export PATH=$HOME/.yarn/bin:$PATH
export PGHOST=127.0.0.1
export PGPASS=postgres
export PGUSER=postgres


alias g=git
alias be='bundle exec'
alias grep='grep --color=auto'
alias tmux='TERM=xterm-256color tmux'
alias v='./venv/bin/'
alias s='twistd -no web --path=.'
alias myip='dig +short myip.opendns.com @resolver1.opendns.com'
alias fixmod='find . -type f -exec chmod 644 {} \; && find . -type d -exec chmod 755 {} \;'
alias pfreeze='pip freeze > requirements.txt'
alias sock5=''


notify() {
  osascript -e "display notification \"$2\" with title \"$1\""
}

ve() {
  exec "./venv/bin/$*"
}

vim() {
  $HOME/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c "$@"
}

e() {
  $HOME/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n "$@"
}

# Tools
faviconize() {
  favicon=favicon

  # if [[ ! -z $BW ]]; then
  #     bw="-colors 256 PNG8:"
  # fi

  # imagemagick_opts="-filter Triangle -define filter:support=2 -unsharp 0.25x0.08+8.3+0.045 -dither None -posterize 136 -quality 82 -define jpeg:fancy-upsampling=off -define png:compression-filter=5 -define png:compression-level=9 -define png:compression-strategy=1 -define png:exclude-chunk=all -interlace none -colorspace sRGB"

  sizes=("16x16" "32x32" "48x48" "64x64" "144x144")
  for size in "${sizes[@]}"; do
    convert $1 $imagemagick_opts -thumbnail $size "$bw$favicon$size.png" #&& optipng -o9 -strip all "favicon$size.png"
  done

  icos="favicon16x16.png favicon32x32.png favicon48x48.png favicon64x64.png"
  # convert $icos -colors 256 favicon.ico
  convert $icos favicon.ico
  rm $icos
}

if [ -d "/Applications/VMware Fusion.app/Contents/Library" ]; then
    export PATH="/Applications/VMware Fusion.app/Contents/Library":$PATH
fi

if [ -d "$HOME/Applications/Emacs.app/Contents/MacOS" ]; then
    export PATH="$HOME/Applications/Emacs.app/Contents/MacOS":$PATH
fi

export RBENV_ROOT="$HOME/.rbenv"
export PATH="$RBENV_ROOT/bin:$PATH"
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

. $LOCAL/vendor/z.sh

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
