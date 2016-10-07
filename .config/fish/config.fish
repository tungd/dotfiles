#! /usr/local/bin/fish
if test -n "$HOME" ;
  set -xg NIX_LINK "$HOME/.nix-profile"

  # Set the default profile.
  if not test -L "$NIX_LINK" ;
    echo "creating $NIX_LINK" >&2
    set -l _NIX_DEF_LINK /nix/var/nix/profiles/default
    ln -s "$_NIX_DEF_LINK" "$NIX_LINK"
  end

  set -xg PATH $NIX_LINK/bin $NIX_LINK/sbin $PATH

  # Subscribe the user to the Nixpkgs channel by default.
  if not test -e $HOME/.nix-channels ;
    echo "https://nixos.org/channels/nixpkgs-unstable nixpkgs" > $HOME/.nix-channels
  end

  # Append ~/.nix-defexpr/channels/nixpkgs to $NIX_PATH so that
  # <nixpkgs> paths work when the user has fetched the Nixpkgs
  # channel.
  # set -xg  NIX_PATH ${NIX_PATH:+$NIX_PATH:}nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs
  set -xg NIX_PATH $NIX_PATH $HOME/.nix-defexpr/channels/nixpkgs:nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs

  # Set $SSL_CERT_FILE so that Nixpkgs applications like curl work.
  if test -e /etc/ssl/certs/ca-certificates.crt
    set -xg SSL_CERT_FILE /etc/ssl/certs/ca-certificates.crt
  else if test -e /etc/ssl/certs/ca-bundle.crt
    set -xg SSL_CERT_FILE /etc/ssl/certs/ca-bundle.crt
  else if test -e /etc/pki/tls/certs/ca-bundle.crt
    set -xg SSL_CERT_FILE /etc/pki/tls/certs/ca-bundle.crt
  else if test -e "$NIX_LINK/etc/ssl/certs/ca-bundle.crt"
    set -xg SSL_CERT_FILE "$NIX_LINK/etc/ssl/certs/ca-bundle.crt"
  else if test -e "$NIX_LINK/etc/ca-bundle.crt"
    set -xg SSL_CERT_FILE "$NIX_LINK/etc/ca-bundle.crt"
  end
end

set fish_color_user red
set fish_color_cwd green
set fish_color_host FFA500

function fish_prompt
  set last_status $status

  set_color $fish_color_user
  printf '\n%s' $USER
  set_color normal

  echo -ne '@'

  set_color $fish_color_host
  printf '%s' (hostname)
  set_color normal

  echo -ne ' in '

  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)
  set_color normal

  set -l sha (git rev-parse --short HEAD 2> /dev/null)
  if test $status = 0
    git diff-files --quiet --ignore-submodules 2>/dev/null
    if test $status = 0
      set_color green
    else
      set_color red
    end
    printf ' [%s:%s]' (git symbolic-ref --short HEAD 2> /dev/null) $sha
    set_color normal
    printf '%s' (git diff --shortstat)
  end

  echo -ne '\n› '
  set_color normal
end
