---
title: "PRD: Emacs Command Workspace project workspaces and durable terminals"
labels:
  - ready-for-agent
status: ready-for-agent
---

## Problem Statement

I want Emacs to become my primary Command Workspace, replacing Warp for daily project tabs, panes, shells, and durable command sessions. Today the dotfiles contain several exploratory agent and terminal integrations, but there is no coherent Project Workspace model, no single command that opens a durable project terminal, and no validated Terminal Backend choice for tmux-backed daily shell work.

The immediate problem is practical: I need to freely close, resume, and cycle Emacs without losing shell state. Longer term, this workspace should support a Workflow Agent CLI that emits structured CLI Agent Events for Emacs notifications, transcript storage, weekly summaries, and future agent context. That future agent architecture depends on whether EAT is good enough as the Terminal Backend; if EAT is too slow or incompatible, vterm remains the fallback.

## Solution

Build the first slice of an Emacs-centered Command Workspace:

- Use tab-bar Project Workspaces, one per project root.
- Open new Project Workspaces on Magit status.
- Add a single Project Terminal command that lazily creates or attaches the project's default tmux-backed Project Session.
- Validate EAT as the preferred Terminal Backend with a focused spike; fall back to vterm if EAT fails acceptance criteria.
- Keep the higher-level Project Workspace and Project Session model independent of the chosen Terminal Backend.
- Remove obsolete local agent wrapper experiments so the future Workflow Agent CLI can be built cleanly.
- Leave Agent Transcript Store, Agent Inbox, Transcript View, and Workflow Agent CLI implementation for later phases, but preserve the domain decisions and ADR already made.

## User Stories

1. As a daily Emacs user, I want Emacs to be my Command Workspace, so that I do not need Warp for normal project shell work.
2. As a daily Emacs user, I want one Project Workspace per project root, so that project context maps cleanly to an Emacs tab.
3. As a daily Emacs user, I want opening a project to switch to an existing Project Workspace when one exists, so that I do not create duplicate project tabs.
4. As a daily Emacs user, I want opening a project to create a Project Workspace when none exists, so that the workflow is one command from project selection to work context.
5. As a daily Emacs user, I want new Project Workspaces to open on Magit status, so that I see the project state before opening shells.
6. As a daily Emacs user, I want Warp panes to map to ordinary Emacs windows, so that I can keep using native Emacs window commands.
7. As a daily Emacs user, I want Project Workspace scoping to be layout-first, so that project buffers are organized without brittle global buffer isolation.
8. As a daily Emacs user, I want a Project Terminal command, so that I can open the current project's shell quickly.
9. As a daily Emacs user, I want the Project Terminal to lazily create its tmux Project Session, so that project switching stays cheap and passive.
10. As a daily Emacs user, I want the Project Terminal to reuse an existing tmux Project Session, so that shell state survives Emacs window, buffer, tab, and process churn.
11. As a daily Emacs user, I want each project to have one default Project Terminal, so that shell sessions do not sprawl.
12. As a daily Emacs user, I want Project Sessions named from the project basename, so that session names are readable.
13. As a daily Emacs user, I want Project Session names to avoid collisions with a stable suffix when needed, so that projects with the same basename remain distinct.
14. As a daily Emacs user, I want the Project Terminal buffer named by project, so that global buffer lists remain understandable.
15. As a daily Emacs user, I want to close and reopen Emacs without losing project shell state, so that I can treat Emacs tabs as disposable views over durable sessions.
16. As a daily Emacs user, I want tmux attach, detach, and resize to work reliably inside the Project Terminal, so that the terminal is safe for daily use.
17. As a daily Emacs user, I want fast command output not to make Emacs sluggish, so that the Command Workspace can replace a dedicated terminal app.
18. As a daily Emacs user, I want common interactive TUIs to work inside the Project Terminal, so that I do not need a separate terminal for normal development tools.
19. As a daily Emacs user, I want copy, search, and scroll behavior to be good enough in the Terminal Surface, so that I can inspect command output comfortably.
20. As a daily Emacs user, I want long-running terminal output not to corrupt cursor or scroll state, so that I can trust the Project Terminal during build, test, and log-heavy work.
21. As a daily Emacs user, I want EAT tried first, so that I get the better Emacs integration if it is viable.
22. As a daily Emacs user, I want vterm as a fallback Terminal Backend, so that the workspace design is not blocked by EAT performance or compatibility issues.
23. As a dotfiles maintainer, I want the Terminal Backend hidden behind a small interface, so that EAT and vterm can be swapped without changing the Project Workspace model.
24. As a dotfiles maintainer, I want obsolete terminal-rendered agent wrappers removed, so that the Emacs config has less experimental surface area.
25. As a dotfiles maintainer, I want obsolete dREPL and ACP experiments removed, so that future agent work does not inherit failed prototypes accidentally.
26. As a future Workflow Agent CLI user, I want tmux OSC passthrough evaluated, so that CLI Agent Events can eventually reach Emacs from durable sessions.
27. As a future Workflow Agent CLI user, I want the Project Terminal spike to check OSC 777 passthrough visibility, so that the future event transport is not surprised by tmux.
28. As a future Workflow Agent CLI user, I want the future agent to emit Warp-compatible structured OSC 777 events, so that it remains usable outside Emacs.
29. As a future Workflow Agent CLI user, I want Emacs to consume those CLI Agent Events later, so that notifications and transcripts are driven by structured data rather than terminal screen scraping.
30. As a future Workflow Agent CLI user, I want Agent Notifications to stay inside Emacs by default, so that desktop notifications are not polluted.
31. As a future Workflow Agent CLI user, I want an Agent Inbox later, so that missed agent activity remains reviewable.
32. As a future Workflow Agent CLI user, I want Transcript Sessions to be separate from live Agent Sessions, so that work history survives short process TTLs.
33. As a future Workflow Agent CLI user, I want a global Agent Transcript Store later, so that weekly summaries can span projects.
34. As a future Workflow Agent CLI user, I want the transcript store to use canonical JSONL plus a rebuildable SQLite index, so that history is both inspectable and queryable.
35. As a future Workflow Agent CLI user, I want read-only Transcript Views before agent resume, so that event history can be useful before live input/resume is designed.
36. As a future Workflow Agent CLI user, I want Agent Tasks to be explicit and titled, so that transcript history does not collapse into one endless project chat.
37. As a future project-management user, I want Project Dashboard work deferred, so that the terminal replacement can land before org/PRD dashboard design is ready.

## Implementation Decisions

- Build a Project Workspace module around Emacs tab-bar. It should create or switch to one tab per project root, rename the tab using the Project Session identity, and open Magit status for newly created workspaces.
- Build a Project Session naming module as a deep module. Its public interface should accept a project root and optional role, return a readable session name, and append a short stable suffix only when needed to avoid collisions.
- Build a tmux adapter as a deep module. Its public interface should cover checking whether a session exists, creating a session in a project root, attaching to a session command, listing sessions, and eventually configuring or verifying passthrough behavior.
- Build a Terminal Backend adapter so the Project Terminal can target EAT first and vterm later without changing workspace/session behavior.
- Install and configure EAT only after the spike confirms it is viable. EAT is preferred, but not assumed to be permanent.
- Keep vterm configured as fallback. If EAT fails critical acceptance criteria, use vterm for Project Terminal while preserving the Project Workspace and Project Session model.
- Implement a Project Terminal command that finds the current project root, computes the default Project Session name, creates the tmux session if absent, and attaches through the selected Terminal Backend.
- The Project Terminal command should be explicit and lazy. Project switching must not spawn shells by itself.
- Each project gets one default Project Terminal for ordinary shell work. Additional named role sessions are not part of this first slice.
- Project Workspace scoping is layout-first. Do not implement strict per-tab buffer isolation in this PRD.
- Remove obsolete local agent experiments, including Codex, Claude Code, vterm-agent, dREPL smolagent, and ACP wrappers. Keep general packages like dREPL or code-cells only if still used outside those wrappers.
- Update tmux configuration comments/settings to reflect the new model once EAT/vterm passthrough behavior is validated.
- Record the future Workflow Agent CLI direction but do not implement it in this slice. The future event transport follows Warp's structured OSC 777 shape with title `warp://cli-agent` and JSON event bodies.
- Respect the existing ADR: future transcript persistence should write a canonical append-only JSONL event log first, then maintain SQLite as a rebuildable Transcript Index.
- Do not introduce Project Dashboard behavior in this slice. Newly created Project Workspaces open on Magit status.

## Testing Decisions

- Good tests should cover external behavior: project root to workspace selection, session naming, tmux command construction, backend command selection, and cleanup of obsolete integrations. Avoid tests that assert private helper structure unless the helper is a deliberately deep module with a stable interface.
- Test the Project Session naming module with basename-only names, duplicate basenames, role-prefixed agent names, non-alphanumeric project names, and stable suffix behavior.
- Test the tmux adapter without requiring live tmux where possible by asserting command construction and parsing of tmux list output. Add a small manual or integration check for real tmux attach/detach behavior.
- Test Project Workspace behavior at the command level: opening a known project creates a tab, reopening the same project switches to the same tab, and a new workspace opens Magit status.
- Test Terminal Backend selection through a small interface so EAT and vterm behavior can be verified independently.
- Test that the Project Terminal command creates a missing Project Session and reuses an existing one.
- Test that removing obsolete wrappers also removes their keybindings and autoload/use-package references.
- Use existing ERT-style tests as prior art; the repo already contains local Emacs package tests for experimental terminal/agent behavior.
- Include a manual EAT spike checklist before committing to EAT: tmux attach/detach, resize, fast output, common TUIs, copy/search/scroll, OSC 777 passthrough visibility, and long-running output stability.

## Out of Scope

- Building the Workflow Agent CLI.
- Building Agent Transcript Store persistence.
- Building the SQLite Transcript Index.
- Building Agent Inbox or Emacs notification UI.
- Building read-only Transcript Views.
- Building editable transcript input, org-mode export, or agent resume.
- Building Project Dashboard from org files and agent-produced PRDs.
- Supporting multiple default shell sessions per project.
- Strict per-tab buffer isolation.
- Desktop notification integration for agent activity.
- Recreating Warp UI behavior literally.

## Further Notes

The design intentionally separates high-level workspace concepts from Terminal Backend choice. EAT is preferred because of Emacs integration, but vterm remains acceptable if EAT is too slow or incompatible. The later agent architecture depends on this spike because OSC event visibility and terminal behavior inside tmux will influence the shape of the Workflow Agent CLI integration.

The repo now has a domain glossary for this Command Workspace and an ADR recording the future transcript-store decision. Future implementation tickets should use those terms rather than generic terms like "daily terminal", "tab", "pane", or "terminal transcript" when they actually mean Command Workspace, Project Workspace, Project Session, or Agent Transcript Store.
