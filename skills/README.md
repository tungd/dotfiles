# Shared LOOP skills

Edit `loop-plan/SKILL.md` and `loop-execute/SKILL.md` here. This directory is the
source for the two LOOP skills; agent-specific installations are symlinks.
OpenCode's slash-command wrappers are in `commands/` and contain no workflow rules.

From the dotfiles checkout:

```sh
python3 skills/install.py
python3 skills/install.py --check
```

The installer links only these two skills into `~/.agents/skills` (read by AA),
`~/.codex/skills`, `~/.claude/skills`, `~/.config/opencode/skills`, and
`~/.config/poolside/skills`. It also preserves `~/.config/agent-skills` as
compatibility links and installs the two OpenCode command links.

Re-running is idempotent. Existing files, directories, and replaced symlinks are
moved to the printed directory under `~/.local/state/loop-skills/backups`, keeping
their paths relative to home. Backups remain outside skill discovery. To restore
one, remove only its corresponding installed symlink and move the saved entry
back to its original path. Other skills are not managed by this installer.

Use `/loop-plan` to write or repair the repository's LOOP, then `/loop-execute`
with that file to implement its remaining work. The plan carries intent, approach,
code anchors, and checks; the executor keeps working across commits. Existing
LOOP formats remain supported. Already running conversations can retain older
instructions in their context; installation checks establish disk state only.
