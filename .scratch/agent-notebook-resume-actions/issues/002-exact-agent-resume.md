---
title: "Exact Agent Resume"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Implement Agent Resume as a first-class Workflow Agent CLI operation. Resume
should continue an existing Transcript Session by session id, reconstruct
Responses provider input exactly from stored Provider Conversation Items, and
append new prompt/response/tool events to the same Agent Task. Resume with an
immediate prompt should run one continuation. Resume without a prompt should
start Interactive Agent Resume: a line-oriented prompt loop, not a full-screen
terminal UI.

This slice should be demoable from the CLI by creating a titled run, resuming it
with a follow-up prompt, and seeing both prompts grouped under the same
Transcript Session.

## Acceptance criteria

- [ ] `td-agent resume SESSION_ID PROMPT` appends a new prompt to the existing Transcript Session.
- [ ] Promptless `td-agent resume SESSION_ID` starts a line-oriented Interactive Agent Resume.
- [ ] Agent Resume reconstructs provider input from Provider Conversation Items, not rendered notebook text.
- [ ] Agent Resume reports a clear error for missing sessions.
- [ ] Agent Resume reports a clear error when exact reconstruction is impossible.
- [ ] `stop` completes the current continuation without permanently closing the Agent Task.
- [ ] `idle_prompt` indicates the same Transcript Session is ready for another prompt.
- [ ] Tests cover successful resume, promptless resume, missing session errors, malformed reconstruction errors, and continuation event semantics.

## Blocked by

- 001-titled-transcript-sessions-with-resume-ready-events
