---
title: "New Agent Notebook runs and continues tasks"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Implement the first end-to-end Agent Notebook path in Emacs for new Agent
Tasks. A user should be able to create a new Agent Notebook from Emacs, submit
an initial prompt, see live output in a Running Notebook Cell, and continue the
same Transcript Session by submitting the next Draft Notebook Cell through
non-interactive Agent Resume. The Agent Notebook is a disposable Notebook
Projection: streamed UI feedback is reconciled from the Canonical Event Log
when a cell completes.

This slice should be demoable entirely from Emacs without starting a
terminal-rendered agent.

## Acceptance criteria

- [ ] Emacs can create a new Agent Notebook that starts a new titled Agent Task.
- [ ] The notebook binds to the emitted Transcript Session id.
- [ ] The notebook renders Draft, Running, and Complete Notebook Cell states for new runs.
- [ ] Prior cells are read-only and exactly one Draft Notebook Cell is editable.
- [ ] Submitting a Draft Notebook Cell invokes non-interactive Agent Resume with the correct session id and prompt.
- [ ] Running cells stream live notebook-launched output or events.
- [ ] Completed cells reconcile from the Canonical Event Log.
- [ ] Tool activity is present but collapsed by default.
- [ ] Tests cover notebook creation, event ingestion, cell state transitions, editability, submission command construction, and transcript reconciliation.

## Blocked by

- 001-titled-transcript-sessions-with-resume-ready-events
- 002-exact-agent-resume
