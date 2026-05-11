---
title: "Permission requests use Agent Action Transient"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
type: AFK
---

## Parent

PRD: Agent Notebook resume and Emacs action handling

## What to build

Connect CLI permission requests to the Emacs Agent Notebook workflow through an
Agent Action Transient. When Emacs ingests a `permission_request` event, the
relevant Notebook Cell should become blocked, the transient should expose
approve, deny, open notebook, and review/dismiss actions, and approve/deny
should call the Permission Reply Command. The notebook should reflect the
eventual `permission_replied` state and continue rendering subsequent events.

This slice should be demoable by triggering a permissioned tool from an Agent
Notebook, approving or denying in Emacs, and seeing the same cell continue or
receive a denial result.

## Acceptance criteria

- [ ] Emacs Agent Event Ingestion recognizes `permission_request` events.
- [ ] A permission request marks the relevant Notebook Cell as blocked.
- [ ] The Agent Action Transient shows tool name, project/session context, and input preview.
- [ ] The transient provides approve, deny, open notebook, and review/dismiss actions.
- [ ] Approve invokes the Permission Reply Command with the correct request id and decision.
- [ ] Deny invokes the Permission Reply Command with the correct request id and decision.
- [ ] `permission_replied` updates the notebook and allows subsequent events to render.
- [ ] Tests cover event ingestion, transient action dispatch, CLI reply invocation, blocked cell rendering, and post-reply notebook updates.

## Blocked by

- 003-new-agent-notebook-runs-and-continues-tasks
- 005-cli-permission-flow-for-permissioned-tools
