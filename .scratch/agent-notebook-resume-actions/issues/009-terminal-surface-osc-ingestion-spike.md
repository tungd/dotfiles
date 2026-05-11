---
title: "Terminal Surface OSC ingestion spike"
labels:
  - enhancement
  - ready-for-human
status: ready-for-human
type: HITL
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Spike whether Terminal Surfaces backed by EAT and tmux can feed the shared Agent
Event Ingestion path from terminal-rendered fallback sessions. The goal is not
to make terminal-rendered agents the primary workflow; it is to validate whether
Warp-compatible OSC 777 `warp://cli-agent` events passing through tmux can be
observed, parsed, and routed into the same Emacs UI path used by
notebook-launched agents.

This is HITL because it requires manual terminal behavior validation with EAT,
tmux, and real Emacs buffers.

## Acceptance criteria

- [ ] A terminal-rendered agent or test emitter can emit Warp-compatible OSC 777 events inside a tmux session.
- [ ] EAT/tmux event behavior is manually validated for event visibility, passthrough, and terminal display side effects.
- [ ] Emacs can route observed terminal-surface events into the shared Agent Event Ingestion path, or the spike records why this is not currently viable.
- [ ] The spike documents whether terminal-surface ingestion should proceed, fall back to process-launched notebook ingestion only, or require a different EAT/tmux integration point.
- [ ] The spike does not change the primary Agent Notebook workflow or make terminal-rendered sessions required for v1.

## Blocked by

- 003-new-agent-notebook-runs-and-continues-tasks
