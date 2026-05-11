# Workflow Agent CLI Uses OCaml

The Workflow Agent CLI will be implemented in OCaml, not Python or Swift, because the daily workflow depends on many lightweight concurrent agent sessions that emit structured events instead of rendering a heavy terminal UI. OCaml keeps the core fast and typed while still being practical for protocol modeling, provider adapters, tool loops, transcript persistence, and later remote execution handoff; Swift remains a possible choice only for future macOS-specific UI integration, not the portable agent core.

The earlier Python `td-agent` spike was discarded as exploration rather than a foundation.
