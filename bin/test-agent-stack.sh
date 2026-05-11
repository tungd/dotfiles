#!/usr/bin/env bash
set -euo pipefail

ROOT="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
TD_AGENT_DIR="${TD_AGENT_DIR:-$HOME/Projects/personal/td-agent}"

section() {
  printf '\n==> %s\n' "$*"
}

section "Emacs notebook ERT"
emacs --batch -Q \
  -L "$ROOT/emacs" \
  -l "$ROOT/emacs/td-command-workspace-tests.el" \
  -f ert-run-tests-batch-and-exit

section "Emacs notebook byte compile"
emacs --batch -Q \
  -L "$ROOT/emacs" \
  --eval "(require 'cl-lib)" \
  --eval "(require 'bytecomp)" \
  --eval "(setq load-prefer-newer t)" \
  --eval "(cl-letf (((symbol-function 'byte-compile-dest-file) (lambda (_) (expand-file-name \"td-command-workspace-test.elc\" temporary-file-directory)))) (byte-compile-file \"$ROOT/emacs/td-command-workspace.el\"))"

section "code-cells integration smoke"
emacs --batch -Q \
  -L "$ROOT/emacs" \
  -l "$ROOT/emacs/td-command-workspace-code-cells-smoke.el"

section "dotfiles diff check"
git -C "$ROOT" diff --check -- \
  .gitignore \
  emacs/td-command-workspace.el \
  emacs/td-command-workspace-tests.el \
  emacs/td-command-workspace-code-cells-smoke.el \
  bin/test-agent-stack.sh

if [[ "${SKIP_TD_AGENT:-0}" == "1" ]]; then
  section "td-agent skipped"
  exit 0
fi

if [[ ! -d "$TD_AGENT_DIR" ]]; then
  section "td-agent skipped"
  printf 'missing td-agent checkout: %s\n' "$TD_AGENT_DIR"
  exit 0
fi

section "td-agent dune test"
(cd "$TD_AGENT_DIR" && opam exec -- dune test)

section "td-agent dune build"
(cd "$TD_AGENT_DIR" && opam exec -- dune build)

section "td-agent no-auth runtime smoke"
empty_home="$(mktemp -d "${TMPDIR:-/tmp}/td-agent-empty-home.XXXXXX")"
agent_home="$(mktemp -d "${TMPDIR:-/tmp}/td-agent-home.XXXXXX")"
trap 'rm -rf "$empty_home" "$agent_home"' EXIT

set +e
HOME="$empty_home" \
TD_AGENT_HOME="$agent_home" \
OPENAI_API_KEY= \
  "$TD_AGENT_DIR/_build/default/bin/main.exe" \
  run --title "Smoke" "Hello" \
  >"$agent_home/no-auth.stdout" \
  2>"$agent_home/no-auth.stderr"
status=$?
set -e

if [[ "$status" -ne 123 ]]; then
  printf 'expected td-agent no-auth run to exit 123, got %s\n' "$status" >&2
  printf '\nstdout:\n' >&2
  cat "$agent_home/no-auth.stdout" >&2
  printf '\nstderr:\n' >&2
  cat "$agent_home/no-auth.stderr" >&2
  exit 1
fi

section "td-agent diff check"
(cd "$TD_AGENT_DIR" && git diff --check)

section "agent stack tests passed"
