---
title: "CLI Permission Flow for Permissioned Agent Tools"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Implement minimal Agent Permission Flow in the Workflow Agent CLI. Read-Only
Agent Tools should run without approval. Permissioned Agent Tools should emit a
permission request, persist pending request state, block execution until an
operator Permission Decision arrives through a Permission Reply Command, record
the reply, and continue the Agent Tool Loop only after approval or denial.
Pending permissions should not auto-approve or auto-deny.

This slice should be demoable from the CLI by triggering a permissioned tool,
replying with approve or deny from a separate command, and observing the blocked
task continue accordingly.

## Acceptance criteria

- [ ] Read-only tools run without permission prompts.
- [ ] Permissioned tools emit `permission_request` before execution.
- [ ] Unknown or unclassified tools require permission by default.
- [ ] Pending permission requests are persisted with request id, session id, tool name, project context, and input preview.
- [ ] Permissioned tool execution blocks until a Permission Decision arrives.
- [ ] The Permission Reply Command accepts request id plus approve or deny.
- [ ] Approving records `permission_replied` and executes the blocked tool call.
- [ ] Denying records `permission_replied` and returns a denial result to the Agent Tool Loop.
- [ ] Pending permissions do not auto-approve or auto-deny on a timer.
- [ ] A blocked Transcript Session remains recoverable through Agent Resume if the live process exits.
- [ ] Tests cover classification, pending request persistence, approve/deny replies, duplicate/stale replies, blocked recovery, and tool-loop behavior.

## Blocked by

- 001-titled-transcript-sessions-with-resume-ready-events
- 002-exact-agent-resume
