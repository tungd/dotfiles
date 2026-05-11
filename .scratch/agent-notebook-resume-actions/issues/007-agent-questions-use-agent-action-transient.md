---
title: "Agent questions use Agent Action Transient"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Handle agent questions through the same Emacs action surface as permissions,
while keeping the reply semantics distinct. A `question_asked` event should
make the Notebook Cell blocked and open an Agent Action Transient with answer,
open notebook, and review/dismiss actions. Answering should submit a normal
Agent Resume prompt to the same Transcript Session, creating the next Notebook
Cell rather than replying through the Permission Reply Command.

This slice should be demoable by ingesting a question event, answering it from
the transient, and seeing the answer continue the same Agent Task as a normal
follow-up prompt.

## Acceptance criteria

- [ ] Emacs Agent Event Ingestion recognizes `question_asked` events.
- [ ] A question event marks the relevant Notebook Cell as blocked.
- [ ] The Agent Action Transient shows the question context and answer action.
- [ ] Answering a question invokes Agent Resume with the same Transcript Session id and answer prompt.
- [ ] Question answers do not use the Permission Reply Command.
- [ ] Opening the notebook from the transient navigates to the relevant Project Workspace and Agent Notebook.
- [ ] Tests cover question event ingestion, blocked cell rendering, answer action dispatch, Agent Resume invocation, and separation from permission reply behavior.

## Blocked by

- 003-new-agent-notebook-runs-and-continues-tasks
- 006-permission-requests-use-agent-action-transient
