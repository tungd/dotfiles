#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  afk.sh [issue-file ...]

Runs Codex non-interactively over ready-for-agent local markdown issues.

Defaults:
  - repo: current git repository
  - issues: .scratch/*/issues/*.md
  - sandbox: workspace-write
  - approval policy: never
  - run logs: .scratch/afk-runs/<timestamp>/

Useful env vars:
  CODEX_MODEL=<model>                 Pass -m to codex exec
  CODEX_PROFILE=<profile>             Pass -p to codex exec
  CODEX_SANDBOX=workspace-write       Pass -s to codex exec
  CODEX_APPROVAL_POLICY=never         Pass -a to codex exec
  CODEX_SEARCH=1                      Enable Codex web search
  COMMIT_EACH=1                       Commit after each successful issue
  ALLOW_DIRTY=1                       Run even if git status is dirty
  ISSUE_DIR=<path>                    Use issues from one directory
  AFK_RUN_DIR=<path>                  Override run log directory
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

if ! command -v codex >/dev/null 2>&1; then
  echo "codex CLI not found on PATH" >&2
  exit 127
fi

if ! repo_root="$(git rev-parse --show-toplevel 2>/dev/null)"; then
  echo "This script must run inside a git repository." >&2
  exit 2
fi
cd "$repo_root"

allow_dirty="${ALLOW_DIRTY:-0}"
if [[ "$allow_dirty" != "1" ]] && [[ -n "$(git status --porcelain)" ]]; then
  cat >&2 <<'EOF'
Refusing to run AFK on a dirty worktree.

Make a baseline commit first, or set ALLOW_DIRTY=1 if you intentionally want
the agent to work on top of the current diff.
EOF
  exit 2
fi

issue_dir="${ISSUE_DIR:-}"
run_root="${AFK_RUN_DIR:-.scratch/afk-runs/$(date +%Y%m%d-%H%M%S)}"
mkdir -p "$run_root"

issues=()
if (( $# > 0 )); then
  for issue in "$@"; do
    issues+=("$issue")
  done
elif [[ -n "$issue_dir" ]]; then
  while IFS= read -r issue; do
    issues+=("$issue")
  done < <(find "$issue_dir" -maxdepth 1 -type f -name '*.md' | sort)
else
  while IFS= read -r issue; do
    issues+=("$issue")
  done < <(find .scratch -path '*/issues/*.md' -type f 2>/dev/null | sort)
fi

if (( ${#issues[@]} == 0 )); then
  echo "No issue files found." >&2
  exit 2
fi

for issue in "${issues[@]}"; do
  if [[ ! -f "$issue" ]]; then
    echo "Issue file does not exist: $issue" >&2
    exit 2
  fi

  if ! grep -q '^Status: ready-for-agent$' "$issue"; then
    echo "Skipping non-ready issue: $issue"
    continue
  fi

  title="$(sed -n '1s/^# //p' "$issue")"
  base="$(basename "$issue" .md)"
  prompt_file="$run_root/$base.prompt.md"
  output_file="$run_root/$base.final.md"
  log_file="$run_root/$base.log"

  cat >"$prompt_file" <<EOF
You are working AFK on one local markdown issue in this repository.

Issue file:
$issue

Task:
Implement this issue end-to-end. Read the issue body first, then read relevant local project context when present:

- docs/agents/issue-tracker.md
- docs/agents/triage-labels.md
- docs/agents/domain.md
- CONTEXT.md
- CONTEXT-MAP.md
- docs/adr/

Rules:
- Treat the issue's acceptance criteria as the contract.
- Implement only this issue and its required narrow integration path.
- Do not implement later blocked issues unless this issue explicitly requires that behavior.
- Respect the repo's glossary and ADRs.
- Preserve unrelated user changes.
- Do not use destructive git commands.
- Add focused tests for externally visible behavior and stable module contracts.
- Run the relevant tests before finishing.
- If a blocker makes the issue impossible, stop and explain the blocker in the final response.

Final response:
- Summarize what changed.
- List tests run.
- List any remaining blockers or follow-up issues.
EOF

  echo "==> AFK issue: $title"
  echo "    issue:  $issue"
  echo "    prompt: $prompt_file"
  echo "    log:    $log_file"

  codex_cmd=(codex -a "${CODEX_APPROVAL_POLICY:-never}")

  if [[ "${CODEX_SEARCH:-0}" == "1" ]]; then
    codex_cmd+=(--search)
  fi

  codex_cmd+=(exec -C "$repo_root" -s "${CODEX_SANDBOX:-workspace-write}" --output-last-message "$output_file")

  if [[ -n "${CODEX_MODEL:-}" ]]; then
    codex_cmd+=(-m "$CODEX_MODEL")
  fi

  if [[ -n "${CODEX_PROFILE:-}" ]]; then
    codex_cmd+=(-p "$CODEX_PROFILE")
  fi

  if [[ "${CODEX_JSON:-0}" == "1" ]]; then
    codex_cmd+=(--json)
  fi

  codex_cmd+=(-)

  "${codex_cmd[@]}" <"$prompt_file" 2>&1 | tee "$log_file"

  if [[ "${COMMIT_EACH:-0}" == "1" ]]; then
    if [[ -n "$(git status --porcelain)" ]]; then
      git add -A
      git commit -m "Implement $title"
    else
      echo "No changes to commit for: $title"
    fi
  fi
done

echo "AFK run complete. Logs: $run_root"
