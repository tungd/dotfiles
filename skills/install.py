#!/usr/bin/env python3
"""Link the two LOOP skills into the supported local agent discovery roots."""

import argparse
from datetime import datetime, timezone
from pathlib import Path
import tempfile


SOURCE = Path(__file__).resolve().parent
NAMES = ("loop-plan", "loop-execute")
ROOTS = (
    ".agents/skills",  # AA discovers this directly.
    ".codex/skills",
    ".claude/skills",
    ".config/opencode/skills",
    ".config/poolside/skills",
    ".config/agent-skills",  # Preserve the former canonical paths.
)


def links(home):
    for root in ROOTS:
        for name in NAMES:
            yield home / root / name, SOURCE / name
    for name in NAMES:
        yield (
            home / ".config/opencode/commands" / f"{name}.md",
            SOURCE / "commands" / f"{name}.md",
        )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="Check links without changing files")
    parser.add_argument("--home", type=Path, default=Path.home(), help="Installation home (default: current user)")
    args = parser.parse_args()
    home = args.home.expanduser().resolve()
    targets = list(links(home))
    for _, source in targets:
        required = source / "SKILL.md" if source.is_dir() else source
        if not required.is_file():
            parser.error(f"Missing source: {required}")

    pending = [(path, source) for path, source in targets
               if not (path.is_symlink() and path.resolve() == source)]
    if args.check:
        for path, _ in pending:
            print(f"MISMATCH {path}")
        if not pending:
            print(f"OK: all {len(targets)} links resolve to {SOURCE}")
        return int(bool(pending))

    backup = None
    for path, source in pending:
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.exists() or path.is_symlink():
            if backup is None:
                backup_root = home / ".local/state/loop-skills/backups"
                backup_root.mkdir(parents=True, exist_ok=True)
                stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ-")
                backup = Path(tempfile.mkdtemp(prefix=stamp, dir=backup_root))
                print(f"Backup: {backup}")
            saved = backup / path.relative_to(home)
            saved.parent.mkdir(parents=True, exist_ok=True)
            path.rename(saved)
        path.symlink_to(source, target_is_directory=source.is_dir())
        print(f"Linked {path} -> {source}")
    print(f"OK: {len(pending)} links installed; {len(targets) - len(pending)} already current")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
