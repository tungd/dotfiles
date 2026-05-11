# Workflow Agent CLI Starts Headless

The first Workflow Agent CLI runtime will be headless and event-native: it owns the agent tool loop, emits CLI Agent Events, and records transcripts, while Emacs owns the primary UI. This deliberately avoids cloning jcode's TUI shape because the Command Workspace problem is terminal rendering noise and session management, not lack of another full-screen agent interface.
