#!/usr/bin/env bash
if [[ -z "${BASH_VERSION:-}" ]]; then
  echo "afk.sh requires bash. Run it directly or with: bash afk.sh" >&2
  exit 2
fi

set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  afk.sh [up] [issue-file ...]
  afk.sh local [issue-file ...]
  afk.sh status [run-id]
  afk.sh pull [run-id]

Runs Codex non-interactively over ready-for-agent local markdown issues.
The default command is remote-first and launches the run on the AFK host.

Defaults:
  - repo: current git repository
  - issues: .scratch/*/issues/*.md
  - sandbox: workspace-write
  - approval policy: never
  - run logs: .scratch/afk-runs/<timestamp>/
  - remote host: td@100.84.248.34
  - remote root: ~/afk
  - remote name: afk
  - remote login shell: zsh

Useful env vars:
  AFK_REMOTE_HOST=td@100.84.248.34     SSH host for remote AFK runs
  AFK_REMOTE_ROOT=~/afk                Remote root for repos, worktrees, logs
  AFK_REMOTE_NAME=afk                  Local git remote name for AFK exchange
  AFK_REMOTE_LOGIN_SHELL=zsh           Login shell used to find tmux/codex
  AFK_STATUS_LINES=40                  Log lines shown by afk.sh status
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

resolve_script_path() {
  local source="${BASH_SOURCE[0]}"

  if [[ "$source" != */* ]]; then
    source="$(command -v "$source")"
  fi

  absolute_path "$source"
}

issue_workspace() {
  local issue="$1"
  local workspace

  workspace="$(frontmatter_value workspace "$issue")"

  if [[ -n "$workspace" && -n "${AFK_WORKSPACE_REWRITE_FROM:-}" && -n "${AFK_WORKSPACE_REWRITE_TO:-}" ]]; then
    case "$workspace" in
      "$AFK_WORKSPACE_REWRITE_FROM")
        workspace="$AFK_WORKSPACE_REWRITE_TO"
        ;;
      "$AFK_WORKSPACE_REWRITE_FROM"/*)
        workspace="$AFK_WORKSPACE_REWRITE_TO/${workspace#"$AFK_WORKSPACE_REWRITE_FROM"/}"
        ;;
    esac
  fi

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

path_is_inside() {
  local candidate="$1"
  local root="$2"

  [[ "$candidate" == "$root" || "$candidate" == "$root"/* ]]
}

repo_relative_path() {
  local path="$1"
  local abs

  abs="$(absolute_path "$path")"
  if ! path_is_inside "$abs" "$repo_root"; then
    echo "Path is outside this repository: $path" >&2
    return 1
  fi

  if [[ "$abs" == "$repo_root" ]]; then
    printf '.\n'
  else
    printf '%s\n' "${abs#"$repo_root"/}"
  fi
}

absolute_dir() {
  local path="$1"

  CDPATH= cd "$path" && pwd -P
}

repo_relative_dir() {
  local path="$1"
  local abs

  abs="$(absolute_dir "$path")"
  if ! path_is_inside "$abs" "$repo_root"; then
    echo "Directory is outside this repository: $path" >&2
    return 1
  fi

  if [[ "$abs" == "$repo_root" ]]; then
    printf '.\n'
  else
    printf '%s\n' "${abs#"$repo_root"/}"
  fi
}

slugify() {
  local value="$1"
  local slug

  slug="$(printf '%s' "$value" | sed -E 's/[^A-Za-z0-9._-]+/-/g; s/^-+//; s/-+$//')"
  if [[ -z "$slug" ]]; then
    slug="run"
  fi
  printf '%s\n' "$slug"
}

shell_join() {
  local parts=()
  local arg
  local quoted

  for arg in "$@"; do
    printf -v quoted '%q' "$arg"
    parts+=("$quoted")
  done

  local IFS=' '
  printf '%s\n' "${parts[*]}"
}

remote_login_ssh() {
  local script="$1"
  local command

  command="$(shell_join "$afk_remote_login_shell" -lc "$script")"
  ssh "$afk_remote_host" "$command"
}

remote_attach_command() {
  printf 'ssh -tt %s '\''%s -lc "tmux attach -t %s"'\''\n' "$afk_remote_host" "$afk_remote_login_shell" "$tmux_session"
}

collect_issues() {
  issues=()
  if (( $# > 0 )); then
    local issue
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
    return 1
  fi
}

ensure_clean_remote_input() {
  if [[ -n "$(git status --porcelain)" ]]; then
    cat >&2 <<'EOF'
Refusing to launch remote AFK from a dirty worktree.

Remote AFK sends committed git state only. Commit or stash local changes first,
or use `afk.sh local` if you intentionally want to work on a dirty checkout.
EOF
    return 1
  fi
}

validate_remote_issues() {
  local issue
  local target_root
  local ready_count=0

  for issue in "${issues[@]}"; do
    if [[ ! -f "$issue" ]]; then
      echo "Issue file does not exist: $issue" >&2
      return 1
    fi

    if ! is_ready_issue "$issue"; then
      continue
    fi

    if has_unresolved_blockers "$issue"; then
      continue
    fi

    target_root="$(issue_workspace "$issue")"
    if ! path_is_inside "$target_root" "$repo_root"; then
      cat >&2 <<EOF
Remote AFK only supports issues targeting the current repository in this slice.

Issue: $issue
Target workspace: $target_root
Repository: $repo_root

Run this issue from its target repository, or use afk.sh local.
EOF
      return 1
    fi

    ready_count=$((ready_count + 1))
  done

  if (( ready_count == 0 )); then
    echo "No ready unblocked issues found for remote AFK." >&2
    return 1
  fi
}

remote_metadata_dir() {
  printf '%s/.scratch/afk-runs/remote\n' "$repo_root"
}

remote_metadata_path() {
  local run_id="$1"
  printf '%s/%s.env\n' "$(remote_metadata_dir)" "$run_id"
}

latest_remote_run_path() {
  printf '%s/latest\n' "$(remote_metadata_dir)"
}

metadata_get() {
  local key="$1"
  local file="$2"

  awk -F= -v wanted="$key" '$1 == wanted {
    sub(/^[^=]*=/, "")
    print
    exit
  }' "$file"
}

resolve_run_id() {
  local run_id="${1:-}"
  local latest

  if [[ -n "$run_id" ]]; then
    printf '%s\n' "$run_id"
    return 0
  fi

  latest="$(latest_remote_run_path)"
  if [[ ! -f "$latest" ]]; then
    echo "No latest remote AFK run recorded." >&2
    return 1
  fi

  sed -n '1p' "$latest"
}

write_remote_metadata() {
  local metadata_file

  mkdir -p "$(remote_metadata_dir)"
  metadata_file="$(remote_metadata_path "$run_id")"

  {
    printf 'run_id=%s\n' "${run_id:-}"
    printf 'created_at=%s\n' "${created_at:-}"
    printf 'remote_launch_log=%s\n' "${remote_launch_log:-}"
    printf 'remote_host=%s\n' "${afk_remote_host:-}"
    printf 'remote_login_shell=%s\n' "${afk_remote_login_shell:-}"
    printf 'remote_name=%s\n' "${afk_remote_name:-}"
    printf 'remote_url=%s\n' "${remote_url:-}"
    printf 'remote_repo=%s\n' "${remote_repo:-}"
    printf 'remote_worktree=%s\n' "${remote_worktree:-}"
    printf 'remote_run_dir=%s\n' "${remote_run_dir:-}"
    printf 'remote_afk_script=%s\n' "${remote_afk_script:-}"
    printf 'remote_log_file=%s\n' "${remote_log_file:-}"
    printf 'remote_status_file=%s\n' "${remote_status_file:-}"
    printf 'tmux_session=%s\n' "${tmux_session:-}"
    printf 'input_branch=%s\n' "${input_branch:-}"
    printf 'result_branch=%s\n' "${result_branch:-}"
    printf 'input_ref=%s\n' "${input_ref:-}"
    printf 'result_ref=%s\n' "${result_ref:-}"
    printf 'local_head=%s\n' "${local_head:-}"
  } >"$metadata_file"

  printf '%s\n' "$run_id" >"$(latest_remote_run_path)"
}

remote_launch_failed() {
  local status=$?
  local message

  trap - ERR
  message="Remote AFK launch failed during: ${afk_current_step:-unknown} (exit $status)"
  echo "$message" >&2
  if [[ -n "${remote_launch_log:-}" ]]; then
    echo "Launch log: $remote_launch_log" >&2
    printf '%s\n' "$message" >>"$remote_launch_log"
  fi
  write_remote_metadata || true
  exit "$status"
}

remote_launch_step() {
  afk_current_step="$1"
  echo "==> $afk_current_step"
  if [[ -n "${remote_launch_log:-}" ]]; then
    printf '==> %s\n' "$afk_current_step" >>"$remote_launch_log"
  fi
}

remote_launch_run() {
  remote_launch_step "$1"
  shift
  "$@" 2>&1 | tee -a "$remote_launch_log"
}

remote_check_prereqs() {
  local script

  script='missing=0; for tool in bash git tmux codex; do if ! command -v "$tool" >/dev/null 2>&1; then echo "missing remote tool: $tool" >&2; missing=1; fi; done; exit "$missing"'
  if ! remote_login_ssh "$script"; then
    echo "Remote AFK prerequisites are not satisfied on $afk_remote_host." >&2
    return 1
  fi
}

remote_home() {
  ssh "$afk_remote_host" bash -s <<'REMOTE'
printf "%s\n" "$HOME"
REMOTE
}

expand_remote_root() {
  local root="$1"
  local home="$2"

  case "$root" in
    "~")
      printf '%s\n' "$home"
      ;;
    "~/"*)
      printf '%s/%s\n' "$home" "${root#"~/"}"
      ;;
    /*)
      printf '%s\n' "$root"
      ;;
    *)
      printf '%s/%s\n' "$home" "$root"
      ;;
  esac
}

ensure_remote_repo() {
  ssh "$afk_remote_host" bash -s -- "$remote_repo" <<'REMOTE'
set -euo pipefail
remote_repo="$1"
mkdir -p "$(dirname "$remote_repo")"
if [[ ! -d "$remote_repo" ]]; then
  git init --bare "$remote_repo"
fi
REMOTE
}

ensure_local_afk_remote() {
  local current_url

  if current_url="$(git remote get-url "$afk_remote_name" 2>/dev/null)"; then
    if [[ "$current_url" != "$remote_url" ]]; then
      cat >&2 <<EOF
Git remote '$afk_remote_name' already exists with a different URL.

Current:  $current_url
Expected: $remote_url
EOF
      return 1
    fi
  else
    git remote add "$afk_remote_name" "$remote_url"
  fi
}

prepare_remote_worktree() {
  ssh "$afk_remote_host" bash -s -- \
    "$remote_repo" \
    "$remote_worktree" \
    "$remote_run_dir" \
    "$remote_runner_script" \
    "$remote_status_file" \
    "$remote_log_file" \
    "$input_branch" \
    "$result_branch" <<'REMOTE'
set -euo pipefail
remote_repo="$1"
remote_worktree="$2"
remote_run_dir="$3"
remote_runner_script="$4"
remote_status_file="$5"
remote_log_file="$6"
input_branch="$7"
result_branch="$8"

mkdir -p "$(dirname "$remote_worktree")" "$remote_run_dir"
if [[ -e "$remote_worktree" ]]; then
  echo "Remote worktree already exists: $remote_worktree" >&2
  exit 2
fi

git clone "$remote_repo" "$remote_worktree"
git -C "$remote_worktree" checkout -B "$result_branch" "origin/$input_branch"
: >"$remote_log_file"
printf 'state=prepared\nexit_status=\nupdated_at=%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >"$remote_status_file"

cat >"$remote_runner_script" <<'AFK_REMOTE_RUNNER'
#!/usr/bin/env bash
set -uo pipefail

run_id="$1"
remote_worktree="$2"
remote_run_dir="$3"
remote_status_file="$4"
remote_log_file="$5"
result_branch="$6"
push_ref="$7"
local_repo_root="$8"
remote_afk_script="$9"
remote_issue_dir="${10}"
shift 10

cd "$remote_worktree" || exit 2
exec > >(tee -a "$remote_log_file") 2>&1

printf 'state=running\nexit_status=\nupdated_at=%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >"$remote_status_file"
echo "AFK remote run $run_id started at $(date)"
echo "Worktree: $remote_worktree"
echo "Result branch: $result_branch"

afk_env=(
  "ALLOW_DIRTY=1"
  "COMMIT_EACH=0"
  "AFK_RUN_DIR=$remote_run_dir/local"
  "AFK_DEFAULT_WORKSPACE=$remote_worktree"
  "AFK_WORKSPACE_REWRITE_FROM=$local_repo_root"
  "AFK_WORKSPACE_REWRITE_TO=$remote_worktree"
)

if [[ -n "$remote_issue_dir" ]]; then
  afk_env+=("ISSUE_DIR=$remote_issue_dir")
fi

afk_status=0
env "${afk_env[@]}" "$remote_afk_script" local "$@" || afk_status=$?

git_status=0
if [[ -n "$(git status --porcelain)" ]]; then
  git add -A || git_status=$?
  if [[ "$git_status" -eq 0 ]]; then
    git \
      -c user.name="${AFK_REMOTE_GIT_USER_NAME:-AFK Runner}" \
      -c user.email="${AFK_REMOTE_GIT_USER_EMAIL:-afk-runner@localhost}" \
      commit -m "AFK result $run_id" || git_status=$?
  fi
else
  echo "No worktree changes to commit."
fi

push_status=0
git push origin "$push_ref" || push_status=$?

final_status="$afk_status"
if [[ "$git_status" -ne 0 ]]; then
  final_status="$git_status"
fi
if [[ "$push_status" -ne 0 ]]; then
  final_status="$push_status"
fi

if [[ "$final_status" -eq 0 ]]; then
  state="succeeded"
else
  state="failed"
fi

printf 'state=%s\nexit_status=%s\nupdated_at=%s\n' "$state" "$final_status" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >"$remote_status_file"
echo "AFK remote run $run_id finished: $state ($final_status)"
echo "Result branch: $result_branch"
echo "This tmux session is left open for inspection. Exit the shell to close it."

exec bash -l
AFK_REMOTE_RUNNER

chmod +x "$remote_runner_script"
REMOTE
}

install_remote_afk_script() {
  local command

  command="$(
    shell_join \
      bash \
      -c \
      'dest="$1"; mkdir -p "$(dirname "$dest")"; cat >"$dest"; chmod +x "$dest"' \
      _ \
      "$remote_afk_script"
  )"
  ssh "$afk_remote_host" "$command" <"$afk_script_path"
}

start_remote_tmux() {
  local tmux_env=()
  local var
  local tmux_shell_command
  local has_session_cmd
  local new_session_cmd
  local script

  for var in \
    CODEX_MODEL \
    CODEX_PROFILE \
    CODEX_SANDBOX \
    CODEX_APPROVAL_POLICY \
    CODEX_SEARCH \
    CODEX_JSON \
    AFK_IGNORE_BLOCKERS \
    AFK_REMOTE_GIT_USER_NAME \
    AFK_REMOTE_GIT_USER_EMAIL; do
    if [[ -n "${!var:-}" ]]; then
      tmux_env+=("$var=${!var}")
    fi
  done

  tmux_shell_command="$(
    shell_join \
      env \
      "${tmux_env[@]}" \
      bash \
      "$remote_runner_script" \
      "$run_id" \
      "$remote_worktree" \
      "$remote_run_dir" \
      "$remote_status_file" \
      "$remote_log_file" \
      "$result_branch" \
      "HEAD:$result_ref" \
      "$repo_root" \
      "$remote_afk_script" \
      "$remote_issue_dir" \
      "${remote_issue_args[@]}"
  )"

  has_session_cmd="$(shell_join tmux has-session -t "$tmux_session")"
  new_session_cmd="$(shell_join tmux new-session -d -s "$tmux_session" -c "$remote_worktree" "$tmux_shell_command")"
  script="$has_session_cmd >/dev/null 2>&1 && { echo $(shell_join "tmux session already exists: $tmux_session") >&2; exit 2; }; $new_session_cmd"
  remote_login_ssh "$script"
}

run_remote_up() {
  local remote_home_dir
  local remote_root
  local repo_slug
  local current_branch
  local branch_slug
  local timestamp
  local issue
  local remote_issue
  local attach_command

  issue_dir="${ISSUE_DIR:-}"
  collect_issues "$@"
  validate_remote_issues

  afk_remote_host="${AFK_REMOTE_HOST:-td@100.84.248.34}"
  afk_remote_name="${AFK_REMOTE_NAME:-afk}"
  afk_remote_login_shell="${AFK_REMOTE_LOGIN_SHELL:-zsh}"

  repo_slug="$(slugify "$(basename "$repo_root")")"
  current_branch="$(git branch --show-current)"
  if [[ -z "$current_branch" ]]; then
    current_branch="detached-$(git rev-parse --short HEAD)"
  fi
  branch_slug="$(slugify "$current_branch")"
  timestamp="$(date +%Y%m%d-%H%M%S)"
  local_head="$(git rev-parse HEAD)"
  run_id="${timestamp}-${branch_slug}-$(git rev-parse --short=8 HEAD)"
  created_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

  input_branch="afk/input/$run_id"
  result_branch="afk/result/$run_id"
  input_ref="refs/heads/$input_branch"
  result_ref="refs/heads/$result_branch"
  tmux_session="afk-${repo_slug}-${run_id}"
  remote_launch_log="$(remote_metadata_dir)/$run_id.launch.log"
  mkdir -p "$(remote_metadata_dir)"
  trap remote_launch_failed ERR

  echo "Remote AFK launch starting."
  echo "  run id: $run_id"
  echo "  repo:   $repo_root"
  echo "  host:   $afk_remote_host"
  echo "  log:    $remote_launch_log"
  {
    printf 'Remote AFK launch starting.\n'
    printf '  run id: %s\n' "$run_id"
    printf '  repo:   %s\n' "$repo_root"
    printf '  host:   %s\n' "$afk_remote_host"
    printf '  log:    %s\n' "$remote_launch_log"
  } >"$remote_launch_log"
  write_remote_metadata

  remote_issue_args=()
  if (( $# > 0 )); then
    remote_launch_step "mapping issue arguments"
    for issue in "$@"; do
      remote_issue="$(repo_relative_path "$issue")"
      remote_issue_args+=("$remote_issue")
    done
  fi

  remote_issue_dir=""
  if [[ -n "$issue_dir" ]]; then
    remote_launch_step "mapping issue directory"
    remote_issue_dir="$(repo_relative_dir "$issue_dir")"
  fi

  if [[ "${AFK_DRY_RUN:-0}" == "1" ]]; then
    cat <<EOF
Remote AFK dry run:
  host:        $afk_remote_host
  remote:      $afk_remote_name
  login shell: $afk_remote_login_shell
  run id:      $run_id
  input ref:   $input_ref
  result ref:  $result_ref
  tmux:        $tmux_session
EOF
    return 0
  fi

  remote_launch_run "checking clean local input" ensure_clean_remote_input
  remote_launch_run "checking remote prerequisites" remote_check_prereqs
  remote_launch_step "resolving remote home"
  remote_home_dir="$(remote_home)"
  printf 'remote_home=%s\n' "$remote_home_dir" >>"$remote_launch_log"
  remote_root="$(expand_remote_root "${AFK_REMOTE_ROOT:-~/afk}" "$remote_home_dir")"
  remote_repo="$remote_root/repos/$repo_slug.git"
  remote_worktree="$remote_root/worktrees/$repo_slug/$run_id"
  remote_run_dir="$remote_root/runs/$repo_slug/$run_id"
  remote_afk_script="$remote_run_dir/afk.sh"
  remote_log_file="$remote_run_dir/run.log"
  remote_status_file="$remote_run_dir/status.env"
  remote_runner_script="$remote_run_dir/run.sh"
  remote_url="$afk_remote_host:$remote_repo"
  write_remote_metadata

  remote_launch_run "creating remote bare repository" ensure_remote_repo
  remote_launch_run "configuring local git remote" ensure_local_afk_remote
  remote_launch_run "pushing input branch" git push "$afk_remote_name" "HEAD:$input_ref"
  remote_launch_run "preparing remote worktree" prepare_remote_worktree
  remote_launch_run "installing AFK runner script on remote" install_remote_afk_script
  remote_launch_run "starting remote tmux session" start_remote_tmux
  remote_launch_run "recording launch metadata" write_remote_metadata
  attach_command="$(remote_attach_command)"
  trap - ERR

  cat <<EOF
Remote AFK run launched.
  run id:   $run_id
  host:     $afk_remote_host
  worktree: $remote_worktree
  tmux:     $tmux_session
  log:      $remote_log_file

Inspect:
  $attach_command
  bin/afk.sh status $run_id

Fetch results when ready:
  bin/afk.sh pull $run_id
EOF
}

run_remote_status() {
  local run_id_arg="${1:-}"
  local metadata_file
  local state_output
  local tmux_state
  local result_output
  local lines
  local attach_command
  local remote_launch_log

  run_id="$(resolve_run_id "$run_id_arg")"
  metadata_file="$(remote_metadata_path "$run_id")"
  if [[ ! -f "$metadata_file" ]]; then
    echo "No metadata found for remote AFK run: $run_id" >&2
    return 1
  fi

  afk_remote_host="$(metadata_get remote_host "$metadata_file")"
  remote_launch_log="$(metadata_get remote_launch_log "$metadata_file")"
  afk_remote_login_shell="$(metadata_get remote_login_shell "$metadata_file")"
  if [[ -z "$afk_remote_login_shell" ]]; then
    afk_remote_login_shell="${AFK_REMOTE_LOGIN_SHELL:-zsh}"
  fi
  remote_repo="$(metadata_get remote_repo "$metadata_file")"
  remote_worktree="$(metadata_get remote_worktree "$metadata_file")"
  remote_afk_script="$(metadata_get remote_afk_script "$metadata_file")"
  remote_log_file="$(metadata_get remote_log_file "$metadata_file")"
  remote_status_file="$(metadata_get remote_status_file "$metadata_file")"
  tmux_session="$(metadata_get tmux_session "$metadata_file")"
  result_ref="$(metadata_get result_ref "$metadata_file")"
  result_branch="$(metadata_get result_branch "$metadata_file")"
  lines="${AFK_STATUS_LINES:-40}"
  case "$lines" in
    ''|*[!0-9]*)
      lines=40
      ;;
  esac

  if [[ -n "$afk_remote_host" && -n "$remote_status_file" ]]; then
    state_output="$(ssh "$afk_remote_host" bash -s -- "$remote_status_file" <<'REMOTE'
status_file="$1"
if [[ -f "$status_file" ]]; then
  cat "$status_file"
else
  echo "state=unknown"
  echo "exit_status="
  echo "updated_at="
fi
REMOTE
)"
  else
    state_output="state=launch-not-initialized
exit_status=
updated_at="
  fi

  if [[ -n "$tmux_session" ]] && remote_login_ssh "$(shell_join tmux has-session -t "$tmux_session")" >/dev/null 2>&1; then
    tmux_state="running"
  else
    tmux_state="not found"
  fi

  if [[ -n "$afk_remote_host" && -n "$remote_repo" && -n "$result_ref" ]]; then
    result_output="$(ssh "$afk_remote_host" bash -s -- "$remote_repo" "$result_ref" <<'REMOTE'
remote_repo="$1"
result_ref="$2"
if git --git-dir "$remote_repo" show-ref --verify --quiet "$result_ref"; then
  git --git-dir "$remote_repo" log --oneline -1 "$result_ref"
else
  echo "(no result branch yet)"
fi
REMOTE
)"
  else
    result_output="(remote result branch not initialized)"
  fi

  if [[ -n "$tmux_session" ]]; then
    attach_command="$(remote_attach_command)"
  else
    attach_command="(tmux session not initialized)"
  fi

  cat <<EOF
Run:      $run_id
Host:     $afk_remote_host
Worktree: $remote_worktree
Tmux:     $tmux_state ($tmux_session)
Result:   $result_branch
Commit:   $result_output
Attach:   $attach_command

Remote state:
$state_output

Last $lines log lines:
EOF

  if [[ -n "$afk_remote_host" && -n "$remote_log_file" ]]; then
    ssh "$afk_remote_host" bash -s -- "$remote_log_file" "$lines" <<'REMOTE'
remote_log_file="$1"
lines="$2"
if [[ -f "$remote_log_file" ]]; then
  tail -n "$lines" "$remote_log_file"
else
  echo "(no remote log yet)"
fi
REMOTE
  else
    echo "(remote log not initialized)"
  fi

  if [[ -n "$remote_launch_log" ]]; then
    cat <<EOF

Last $lines local launch log lines:
EOF
    if [[ -f "$remote_launch_log" ]]; then
      tail -n "$lines" "$remote_launch_log"
    else
      echo "(local launch log missing: $remote_launch_log)"
    fi
  fi
}

run_remote_pull() {
  local run_id_arg="${1:-}"
  local metadata_file

  run_id="$(resolve_run_id "$run_id_arg")"
  metadata_file="$(remote_metadata_path "$run_id")"
  if [[ ! -f "$metadata_file" ]]; then
    echo "No metadata found for remote AFK run: $run_id" >&2
    return 1
  fi

  afk_remote_name="$(metadata_get remote_name "$metadata_file")"
  remote_url="$(metadata_get remote_url "$metadata_file")"
  result_ref="$(metadata_get result_ref "$metadata_file")"
  result_branch="$(metadata_get result_branch "$metadata_file")"

  if [[ -z "$remote_url" || -z "$result_ref" || -z "$result_branch" ]]; then
    echo "Remote result branch is not initialized for run: $run_id" >&2
    return 1
  fi

  ensure_local_afk_remote

  if ! git ls-remote --exit-code "$remote_url" "$result_ref" >/dev/null 2>&1; then
    echo "Remote result branch does not exist yet: $result_branch" >&2
    return 1
  fi

  git fetch "$afk_remote_name" "$result_ref:$result_ref"
  git log --oneline -1 "$result_branch"

  cat <<EOF
Fetched remote AFK result branch: $result_branch

Inspect:
  git show --stat $result_branch

Apply manually when ready:
  git merge $result_branch
EOF
}

run_local() {
  local issue
  local issue_abs
  local target_root
  local title
  local base
  local prompt_file
  local output_file
  local log_file
  local codex_cmd

  if ! command -v codex >/dev/null 2>&1; then
    echo "codex CLI not found on PATH" >&2
    exit 127
  fi

  allow_dirty="${ALLOW_DIRTY:-0}"
  ensure_clean_workspace "$repo_root" "coordinator"

  issue_dir="${ISSUE_DIR:-}"
  run_root="${AFK_RUN_DIR:-.scratch/afk-runs/$(date +%Y%m%d-%H%M%S)}"
  mkdir -p "$run_root"
  run_root="$(CDPATH= cd "$run_root" && pwd -P)"

  collect_issues "$@"

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
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

afk_script_path="$(resolve_script_path)"

if ! repo_root="$(git rev-parse --show-toplevel 2>/dev/null)"; then
  echo "This script must run inside a git repository." >&2
  exit 2
fi
cd "$repo_root"

command_name="${1:-up}"
case "$command_name" in
  up|local|status|pull)
    shift
    ;;
  *)
    command_name="up"
    ;;
esac

case "$command_name" in
  up)
    run_remote_up "$@"
    ;;
  local)
    run_local "$@"
    ;;
  status)
    run_remote_status "$@"
    ;;
  pull)
    run_remote_pull "$@"
    ;;
esac
