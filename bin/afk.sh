#!/usr/bin/env bash
if [[ -z "${BASH_VERSION:-}" ]]; then
  echo "afk.sh requires bash. Run it directly or with: bash afk.sh" >&2
  exit 2
fi

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
  AFK_DRY_RUN=1                       Print ready issues without running Codex
  AFK_IGNORE_BLOCKERS=1               Run ready issues even with unresolved blockers
  AFK_DEFAULT_WORKSPACE=<path>        Workspace for issues without frontmatter
  ISSUE_DIR=<path>                    Use issues from one directory
  AFK_RUN_DIR=<path>                  Override run log directory

Issue frontmatter:
  workspace: /path/to/workspace       Run Codex in this workspace for the issue
EOF
}

is_ready_issue() {
  local issue="$1"
  grep -Eq '^[Ss]tatus:[[:space:]]*ready-for-agent[[:space:]]*$' "$issue"
}

issue_title() {
  local issue="$1"
  local title

  title="$(sed -n '1s/^# //p' "$issue")"
  if [[ -n "$title" ]]; then
    printf '%s\n' "$title"
    return
  fi

  title="$(sed -n 's/^title:[[:space:]]*//p' "$issue" | head -n 1)"
  if [[ -n "$title" ]]; then
    title="${title%\"}"
    title="${title#\"}"
    title="${title%\'}"
    title="${title#\'}"
    printf '%s\n' "$title"
    return
  fi

  basename "$issue" .md
}

issue_key() {
  basename "$1" .md
}

frontmatter_value() {
  local key="$1"
  local issue="$2"
  local value

  value="$(awk -v wanted="$key" '
    BEGIN { wanted = tolower(wanted) }
    {
      split($0, parts, ":")
      key = tolower(parts[1])
      if (key == wanted) {
        sub(/^[^:]*:[[:space:]]*/, "")
        print
        exit
      }
    }
  ' "$issue")"
  value="${value%%#*}"
  value="${value%"${value##*[![:space:]]}"}"
  value="${value#"${value%%[![:space:]]*}"}"
  value="${value%\"}"
  value="${value#\"}"
  value="${value%\'}"
  value="${value#\'}"
  printf '%s\n' "$value"
}

issue_status() {
  frontmatter_value status "$1"
}

is_complete_issue() {
  local status

  status="$(issue_status "$1")"
  case "$status" in
    done|complete|completed|closed|merged)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

issue_blockers() {
  local issue="$1"

  awk '
    BEGIN { in_blocked_by = 0 }
    /^## Blocked by[[:space:]]*$/ { in_blocked_by = 1; next }
    in_blocked_by && /^## / { exit }
    in_blocked_by {
      line = $0
      gsub(/\r/, "", line)
      sub(/^[[:space:]]*[-*][[:space:]]*/, "", line)
      sub(/[[:space:]]+#.*/, "", line)
      sub(/^[[:space:]]+/, "", line)
      sub(/[[:space:]]+$/, "", line)
      lowered = tolower(line)
      if (line == "" || lowered ~ /^none([[:space:]]|$)/) {
        next
      }
      print line
    }
  ' "$issue"
}

resolve_blocker_issue() {
  local issue="$1"
  local blocker="$2"
  local blocker_key
  local candidate

  blocker_key="$(basename "$blocker" .md)"

  for candidate in "${issues[@]}"; do
    if [[ "$(issue_key "$candidate")" == "$blocker_key" ]]; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done

  candidate="$(dirname "$issue")/$blocker_key.md"
  if [[ -f "$candidate" ]]; then
    printf '%s\n' "$candidate"
    return 0
  fi

  return 1
}

has_unresolved_blockers() {
  local issue="$1"
  local blocker
  local blocker_issue
  local unresolved=()

  if [[ "${AFK_IGNORE_BLOCKERS:-0}" == "1" ]]; then
    return 1
  fi

  while IFS= read -r blocker; do
    [[ -n "$blocker" ]] || continue

    if ! blocker_issue="$(resolve_blocker_issue "$issue" "$blocker")"; then
      unresolved+=("$blocker (missing)")
      continue
    fi

    if ! is_complete_issue "$blocker_issue"; then
      unresolved+=("$(issue_key "$blocker_issue") ($(issue_status "$blocker_issue"))")
    fi
  done < <(issue_blockers "$issue")

  if (( ${#unresolved[@]} == 0 )); then
    return 1
  fi

  echo "Skipping blocked issue: $issue"
  printf '    blocked by: %s\n' "${unresolved[@]}"
  return 0
}

absolute_path() {
  local path="$1"
  local dir
  local base

  dir="$(dirname "$path")"
  base="$(basename "$path")"
  printf '%s/%s\n' "$(CDPATH= cd "$dir" && pwd -P)" "$base"
}

issue_workspace() {
  local issue="$1"
  local workspace

  workspace="$(frontmatter_value workspace "$issue")"
  if [[ -z "$workspace" ]]; then
    workspace="${AFK_DEFAULT_WORKSPACE:-$repo_root}"
  fi

  if [[ "$workspace" != /* ]]; then
    workspace="$repo_root/$workspace"
  fi

  if [[ ! -d "$workspace" ]]; then
    echo "Workspace does not exist for issue $issue: $workspace" >&2
    return 1
  fi

  CDPATH= cd "$workspace" && pwd -P
}

ensure_clean_workspace() {
  local workspace="$1"
  local label="$2"

  if [[ "$allow_dirty" == "1" ]]; then
    return 0
  fi

  if ! git -C "$workspace" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    return 0
  fi

  if [[ -n "$(git -C "$workspace" status --porcelain)" ]]; then
    cat >&2 <<EOF
Refusing to run AFK on a dirty $label worktree:
$workspace

Make a baseline commit first, or set ALLOW_DIRTY=1 if you intentionally want
the agent to work on top of the current diff.
EOF
    return 1
  fi
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
ensure_clean_workspace "$repo_root" "coordinator"

issue_dir="${ISSUE_DIR:-}"
run_root="${AFK_RUN_DIR:-.scratch/afk-runs/$(date +%Y%m%d-%H%M%S)}"
mkdir -p "$run_root"
run_root="$(CDPATH= cd "$run_root" && pwd -P)"

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

  if ! is_ready_issue "$issue"; then
    echo "Skipping non-ready issue: $issue"
    continue
  fi

  if has_unresolved_blockers "$issue"; then
    continue
  fi

  issue_abs="$(absolute_path "$issue")"
  target_root="$(issue_workspace "$issue")"
  ensure_clean_workspace "$target_root" "target"

  title="$(issue_title "$issue")"
  base="$(basename "$issue" .md)"
  prompt_file="$run_root/$base.prompt.md"
  output_file="$run_root/$base.final.md"
  log_file="$run_root/$base.log"

  cat >"$prompt_file" <<EOF
You are working AFK on one local markdown issue in this repository.

Issue file:
$issue_abs

Target workspace:
$target_root

Issue body:
EOF
  sed 's/^/> /' "$issue_abs" >>"$prompt_file"
  cat >>"$prompt_file" <<'EOF'

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
- Work in the target workspace above. The issue file may live in a separate coordinator repo.
- Implement only this issue and its required narrow integration path.
- Do not implement later blocked issues unless this issue explicitly requires that behavior.
- Respect the repo's glossary and ADRs.
- Preserve unrelated user changes.
- Do not use destructive git commands.
- Use TDD for the issue: write or update focused failing tests for the acceptance criteria first, then implement until they pass.
- Test externally visible behavior and stable module contracts, not implementation details.
- Run the relevant tests before finishing.
- If a blocker makes the issue impossible, stop and explain the blocker in the final response.

Final response:
- Summarize what changed.
- List tests run.
- List any remaining blockers or follow-up issues.
EOF

  echo "==> AFK issue: $title"
  echo "    issue:  $issue"
  echo "    target: $target_root"
  echo "    prompt: $prompt_file"
  echo "    log:    $log_file"

  if [[ "${AFK_DRY_RUN:-0}" == "1" ]]; then
    echo "    dry-run: would run Codex"
    continue
  fi

  codex_cmd=(codex -a "${CODEX_APPROVAL_POLICY:-never}")

  if [[ "${CODEX_SEARCH:-0}" == "1" ]]; then
    codex_cmd+=(--search)
  fi

  codex_cmd+=(exec -C "$target_root" -s "${CODEX_SANDBOX:-workspace-write}" --output-last-message "$output_file")

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
    if git -C "$target_root" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
      if [[ -n "$(git -C "$target_root" status --porcelain)" ]]; then
        git -C "$target_root" add -A
        git -C "$target_root" commit -m "Implement $title"
      else
        echo "No changes to commit for: $title"
      fi
    else
      echo "COMMIT_EACH skipped for non-git workspace: $target_root"
    fi
  fi
done

echo "AFK run complete. Logs: $run_root"
