---
title: "Agent Action Queue and Emacs-local notifications"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Implement the first Agent Inbox slice as an Agent Action Queue plus minimal
Emacs-local notifications. The queue should list pending permission requests,
agent questions, completed cells, and errored sessions. Selecting an item should
open the relevant Project Workspace and Agent Notebook; actionable items should
also open the correct Agent Action Transient. Notifications should remain inside
Emacs, fall back to messages plus queue entries if popup support is unavailable,
and never use desktop notifications.

This slice should be demoable by running agent work that completes, blocks on a
permission request, asks a question, and errors; each state should be visible
and actionable from the queue or local notification.

## Acceptance criteria

- [ ] Agent Action Queue lists pending permission requests.
- [ ] Agent Action Queue lists agent questions.
- [ ] Agent Action Queue lists completed cells.
- [ ] Agent Action Queue lists errored sessions.
- [ ] Selecting a queue item opens the relevant Project Workspace and Agent Notebook.
- [ ] Selecting an actionable queue item opens the appropriate Agent Action Transient.
- [ ] Minimal Emacs-local notifications are shown for actionable, completed, and errored events.
- [ ] Notification activation opens the relevant Project Workspace and Agent Notebook.
- [ ] Notification activation opens the relevant Agent Action Transient when applicable.
- [ ] Notification fallback uses Emacs messages plus queue entries when popup support is unavailable.
- [ ] No desktop notifications are produced.
- [ ] Opening a Transcript Session marks relevant notifications as read.
- [ ] Tests cover queue contents, filtering, activation, notification fallback, no-desktop behavior, and read-state clearing.

## Blocked by

- 004-open-existing-agent-notebooks-from-transcript-sessions
- 006-permission-requests-use-agent-action-transient
- 007-agent-questions-use-agent-action-transient
