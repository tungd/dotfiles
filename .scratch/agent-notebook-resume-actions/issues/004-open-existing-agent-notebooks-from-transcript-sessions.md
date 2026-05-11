---
title: "Open existing Agent Notebooks from Transcript Sessions"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Add the existing-session Agent Notebook path. From a project workspace, the user
should be able to select an existing Transcript Session, open it as a Notebook
Projection, review read-only prior Notebook Cells, and continue the same Agent
Task from one editable Draft Notebook Cell. Opening a Transcript Session should
also clear relevant unread/read-state metadata used by later notifications and
the Agent Action Queue.

This slice should be demoable by opening a session created in an earlier Emacs
or CLI run and submitting a follow-up prompt from the notebook.

## Acceptance criteria

- [ ] Emacs can list existing Transcript Sessions for the current project.
- [ ] The selector shows useful task title, project, and session metadata.
- [ ] Opening an existing session renders prior prompt/result cells read-only.
- [ ] Opening an existing session creates or exposes one editable Draft Notebook Cell when the session is ready for input.
- [ ] Submitting the draft continues the existing Transcript Session through Agent Resume.
- [ ] Opening a Transcript Session marks relevant Agent Notifications as read.
- [ ] Tests cover session selection data, rendering old cells, read-only/editable boundaries, follow-up submission, and read-state clearing.

## Blocked by

- 002-exact-agent-resume
- 003-new-agent-notebook-runs-and-continues-tasks
