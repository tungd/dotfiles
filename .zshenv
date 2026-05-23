# . "$HOME/.cargo/env"

if [[ ! -o interactive ]]; then
  case ":$PATH:" in
    *:/usr/local/bin:*) ;;
    *) [[ -x /usr/libexec/path_helper ]] && eval "$(/usr/libexec/path_helper -s)" ;;
  esac
fi
