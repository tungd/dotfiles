---
status: superseded by ADR-0015
---

# Agent Permission Flow Is Deferred

The first Workflow Agent CLI will not ask for operator approval before mutating tools or shell commands execute. This keeps v1 focused on OAuth, Responses, the jcode-shaped tool loop, transcript events, and Emacs integration; tool calls and results are still recorded as CLI Agent Events so the transcript remains auditable, and permission request/reply events remain reserved protocol vocabulary for a later slice.
