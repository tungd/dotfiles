---
title: "Titled Transcript Sessions with resume-ready events"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Make new Workflow Agent CLI runs create titled Agent Tasks and Transcript
Sessions whose canonical events are sufficient foundation for later Agent
Resume. A new run should carry task title metadata, session identity, project
context, compact Warp v1-compatible live CLI Agent Events, and canonical
Provider Conversation Items in the Agent Transcript Store. The Transcript Index
should make sessions discoverable by project, session id, event type, and task
title metadata.

This slice should be demoable by starting a new agent run with an explicit title
and by inspecting the resulting transcript/index metadata without implementing
the resume command yet.

## Acceptance criteria

- [ ] New runs can create Agent Tasks with explicit titles.
- [ ] New runs derive a useful short title from the first prompt when no title is supplied.
- [ ] New runs persist session id, task title, project context, event type, timestamp, and schema version in canonical events.
- [ ] Canonical events include Provider Conversation Items needed by later exact resume.
- [ ] Compact live CLI Agent Events remain Warp v1-compatible and omit full Provider Conversation Items.
- [ ] The Transcript Index supports discovering sessions by session id, project, event type, and task title metadata.
- [ ] Tests cover explicit titles, derived titles, canonical event shape, compact live event shape, and transcript/index lookup.

## Blocked by

None - can start immediately
