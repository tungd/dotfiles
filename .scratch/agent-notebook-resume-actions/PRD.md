---
title: "PRD: Agent Notebook resume and Emacs action handling"
labels:
  - enhancement
  - ready-for-agent
status: ready-for-agent
---

## Problem Statement

I want the Command Workspace to support coding-agent work as an Agent Notebook,
not as a noisy terminal transcript. Terminal-rendered agents spam fast spinners,
fight terminal scrolling, and make Emacs sluggish or awkward even when EAT is
usable for normal interactive shells. The first Workflow Agent CLI can already
run a real Responses-style tool loop and emit CLI Agent Events, but the daily
workflow is still incomplete: each run starts a new Transcript Session, the
notebook surface is only a prototype, permission and question events do not yet
have an Emacs-native action path, and missed agent activity is not recoverable
from a focused Agent Action Queue.

The next slice needs to make a Transcript Session usable as an ongoing Agent
Task. Past prompt/result cells should be read-only, the latest prompt should be
editable, submitting it should continue the same Agent Task through Agent
Resume, and Emacs should react to Universal Agent Support Channel events without
depending on terminal screen scraping. Permission requests and agent questions
should be handled inside Emacs through Transient and local notifications, not
through desktop notifications or terminal prompts.

## Solution

Build the next Command Workspace slice in three vertical parts.

First, implement Agent Resume in the Workflow Agent CLI. New runs create titled
Agent Tasks and Transcript Sessions; resume continues an existing Transcript
Session by session id, either with an immediate prompt or with an interactive
line-oriented prompt loop. The Canonical Event Log stores the Provider
Conversation Items needed to reconstruct the Responses provider input exactly,
while the live Universal Agent Support Channel remains compact and
Warp-compatible.

Second, implement the Agent Notebook as a disposable Notebook Projection in
Emacs. It can open an existing Transcript Session or create a new Agent Task.
It renders Notebook Cells from the Agent Transcript Store, keeps prior cells
read-only, exposes one Draft Notebook Cell, submits the draft through
non-interactive Agent Resume, streams live output during the Running Notebook
Cell, and reconciles completed cells from the Canonical Event Log.

Third, implement Emacs action handling. Permission requests and agent questions
open an Agent Action Transient. Permission decisions are sent back through a
Permission Reply Command; questions are answered as normal Agent Resume prompts.
The Agent Inbox starts as an Agent Action Queue for pending, unread, blocked,
completed, and errored agent activity. Agent Notifications stay Emacs-local and
activate the relevant Project Workspace, Agent Notebook, and action transient.

## User Stories

1. As a Command Workspace user, I want an Agent Notebook for each Transcript Session, so that coding-agent work is managed in Emacs instead of a full terminal transcript.
2. As a Command Workspace user, I want a new Agent Notebook to create a new Agent Task, so that I can start agent work without opening a terminal-rendered agent.
3. As a Command Workspace user, I want an Agent Notebook to open an existing Transcript Session, so that I can resume prior work from history.
4. As a Command Workspace user, I want Agent Tasks to have titles, so that agent history is not a list of opaque session ids.
5. As a Command Workspace user, I want a title to be derived from the first prompt when I do not provide one, so that quick tasks remain lightweight.
6. As a Command Workspace user, I want task titles stored in CLI Agent Events, so that selectors, notebooks, inbox entries, and summaries show meaningful names.
7. As a Command Workspace user, I want one Transcript Session per Agent Task, so that follow-up prompts stay grouped with the work they belong to.
8. As a Command Workspace user, I want `td-agent run` to create a new Transcript Session, so that new work is explicitly separated from existing work.
9. As a Command Workspace user, I want `td-agent resume SESSION_ID PROMPT` to continue an existing Transcript Session, so that follow-up prompts preserve task context.
10. As a Command Workspace user, I want `td-agent resume SESSION_ID` to start an interactive line-oriented resume, so that terminal fallback remains possible without a TUI.
11. As a Command Workspace user, I want resume to be owned by the Workflow Agent CLI, so that Emacs does not reconstruct model context itself.
12. As a Command Workspace user, I want Agent Resume to replay exact Provider Conversation Items, so that resumed work continues from the same provider context.
13. As a Command Workspace user, I want resume to fail clearly if exact reconstruction is impossible, so that the agent never silently starts a new task.
14. As a Command Workspace user, I want past notebook cells to be read-only, so that the notebook remains a faithful projection of the transcript.
15. As a Command Workspace user, I want only the latest prompt cell to be editable, so that the interaction model is clear and constrained.
16. As a Command Workspace user, I want a Draft Notebook Cell to become a Running Notebook Cell when submitted, so that I can see when the agent owns the turn.
17. As a Command Workspace user, I want a Running Notebook Cell to become complete on `stop`, so that each prompt/result boundary is explicit.
18. As a Command Workspace user, I want `idle_prompt` to create the next Draft Notebook Cell, so that I can keep working in the same Agent Task.
19. As a Command Workspace user, I want `stop` to mean the current continuation finished, so that a stopped cell does not close the whole Agent Task.
20. As a Command Workspace user, I want permanent Agent Task closure to be separate from `stop`, so that multi-prompt tasks remain natural.
21. As a Command Workspace user, I want prompts and responses visible by default, so that notebooks are readable as work logs.
22. As a Command Workspace user, I want tool activity collapsed by default, so that audits are available without overwhelming the notebook.
23. As a Command Workspace user, I want permission requests and questions shown prominently, so that actionable agent state is not buried in tool logs.
24. As a Command Workspace user, I want notebook output to stream while the agent runs, so that I get immediate feedback.
25. As a Command Workspace user, I want completed cells reconciled from the Canonical Event Log, so that live streaming glitches do not corrupt history.
26. As a Command Workspace user, I want the Canonical Event Log to remain the source of truth, so that notebooks are disposable projections.
27. As a Command Workspace user, I want the Agent Transcript Store to remain global, so that weekly review and cross-project search still work.
28. As a Command Workspace user, I want the Transcript Index to make session lookup fast, so that opening an existing notebook does not require manual log scanning.
29. As a Command Workspace user, I want live OSC events to stay compact, so that tmux and terminal surfaces are not overloaded with large provider payloads.
30. As a Command Workspace user, I want full Provider Conversation Items stored only in the Canonical Event Log, so that resume is reliable without making live UI events huge.
31. As a Command Workspace user, I want CLI Agent Events to stay Warp v1-compatible, so that the Workflow Agent CLI remains usable in Warp and Emacs.
32. As a Command Workspace user, I want Emacs to tolerate extra event fields, so that transcript reconstruction can evolve without forking the event channel.
33. As a Command Workspace user, I want one Agent Event Ingestion path in Emacs, so that notebook-launched agents and terminal-surface agents behave consistently.
34. As a Command Workspace user, I want notebook-launched agents to be the required ingestion source for this slice, so that Agent Notebook v1 is not blocked on EAT/tmux interception.
35. As a Command Workspace user, I want Terminal Surface OSC ingestion to remain a compatibility spike, so that terminal-rendered fallback sessions can be integrated later.
36. As a Command Workspace user, I want mutating tool calls to require approval, so that agent autonomy is controlled from Emacs.
37. As a Command Workspace user, I want read-only tools to run without approval, so that normal inspection remains fast.
38. As a Command Workspace user, I want unknown tools to require approval, so that missing classification fails conservatively.
39. As a Command Workspace user, I want permission requests to open an Agent Action Transient, so that approval happens in Emacs rather than the terminal.
40. As a Command Workspace user, I want permission details to include tool name, project context, and input preview, so that I can make a quick decision.
41. As a Command Workspace user, I want to approve a permission request from the transient, so that the agent can continue the blocked tool call.
42. As a Command Workspace user, I want to deny a permission request from the transient, so that the agent receives an explicit refusal.
43. As a Command Workspace user, I want permission replies delivered through a CLI command, so that Emacs does not need to own process stdin.
44. As a Command Workspace user, I want pending permissions to remain blocked indefinitely, so that unattended work does not auto-approve or auto-deny.
45. As a Command Workspace user, I want a blocked Transcript Session to remain resumable if the live process exits, so that process TTL does not lose decision state.
46. As a Command Workspace user, I want agent questions to open an Agent Action Transient, so that questions are handled with the same Emacs action surface.
47. As a Command Workspace user, I want answering an agent question to submit an Agent Resume prompt, so that questions become normal Notebook Cells.
48. As a Command Workspace user, I want permission flow and question flow to be visibly distinct, so that tool approval is not confused with new instructions.
49. As a Command Workspace user, I want an Agent Action Queue, so that I can recover missed blocked, errored, completed, and unread agent activity.
50. As a Command Workspace user, I want selecting an Agent Action Queue item to open the relevant Agent Notebook, so that review starts in the right work context.
51. As a Command Workspace user, I want selecting an actionable queue item to open its Agent Action Transient, so that pending work can be resolved quickly.
52. As a Command Workspace user, I want Agent Notifications to stay inside Emacs, so that desktop notifications remain uncluttered.
53. As a Command Workspace user, I want notification activation to switch to the relevant Project Workspace, so that action lands in the correct project context.
54. As a Command Workspace user, I want notification activation to open the relevant Agent Notebook, so that I can inspect the transcript before acting.
55. As a Command Workspace user, I want notification activation to open the relevant action transient when needed, so that permission and question events are directly actionable.
56. As a Command Workspace user, I want read state to be marked when a Transcript Session is opened, so that opening the notebook clears relevant unread status.
57. As a Workflow Agent CLI maintainer, I want resume reconstruction isolated behind a deep module, so that provider replay can be tested without launching Emacs.
58. As a Workflow Agent CLI maintainer, I want permission coordination isolated behind a deep module, so that pending request persistence and reply handling can be tested independently.
59. As a Workflow Agent CLI maintainer, I want the event schema builder to remain small and explicit, so that Warp compatibility is easy to verify.
60. As a Workflow Agent CLI maintainer, I want the tool permission classifier separated from tool execution, so that tool policy changes do not rewrite tool implementations.
61. As an Emacs integrator, I want notebook rendering separated from process management, so that transcript rendering can be tested without running the agent.
62. As an Emacs integrator, I want event ingestion separated from notification and action presentation, so that all event sources share the same validation path.
63. As an Emacs integrator, I want Transient commands separated from notebook rendering, so that permission UI can evolve without changing transcript projection.
64. As a future remote execution user, I want resume and permissions to address sessions and request ids, so that remote handoff can reuse the same control vocabulary later.
65. As a future project-management user, I want notebooks and Agent Tasks to use the same Task Anchor vocabulary later, so that PRDs, issues, and org headings can connect to transcripts.

## Implementation Decisions

- Implement the next work as three vertical slices: Agent Resume core, Agent Notebook projection, and Emacs action handling.
- Treat the Agent Notebook as a Notebook Projection, not as a canonical org document or second transcript store.
- Keep the Agent Transcript Store global. The Canonical Event Log remains append-only JSONL, and the Transcript Index remains rebuildable SQLite.
- Keep one global Canonical Event Log for v1. Do not introduce per-session canonical log files.
- Store enough Provider Conversation Items in the Canonical Event Log to reconstruct a Responses continuation exactly.
- Do not summarize or compress transcript history before resume works. Summarization is a later optimization.
- Add Agent Resume as a first-class Workflow Agent CLI operation. Emacs passes session identity and prompts; the CLI reconstructs provider input and owns the Agent Tool Loop.
- The resume command takes a Transcript Session id as the stable identifier.
- The resume prompt is optional. With a prompt, the CLI runs one continuation. Without a prompt, the CLI starts Interactive Agent Resume and reads line-oriented prompts from standard input.
- New runs create new Agent Tasks and Transcript Sessions. Resume appends to an existing Transcript Session.
- New runs accept an Agent Task title. If no title is supplied, the Workflow Agent CLI may derive a short title from the first operator prompt.
- Task title, session id, cwd, project, event type, event id, timestamp, and schema version are stored in CLI Agent Events.
- CLI Agent Events stay compatible with Warp's Universal Agent Support Channel v1. Use OSC 777 with `warp://cli-agent`, JSON body, `v: 1`, `agent`, `event`, `session_id`, `cwd`, `project`, and the known display fields.
- Additional fields for Emacs and transcript reconstruction are allowed only when they are ignorable by Warp-compatible terminals.
- Live Universal Agent Support Channel events carry compact UI-oriented fields. Full Provider Conversation Items are persisted in the Canonical Event Log, not sent wholesale over OSC.
- Emit `prompt_submit`, `tool_complete`, `stop`, `idle_prompt`, `permission_request`, `permission_replied`, and `question_asked` events as the core notebook/action event vocabulary.
- Interpret `stop` as the end of the current Notebook Cell continuation, not permanent Agent Task closure.
- Interpret `idle_prompt` as readiness for another operator prompt in the same Transcript Session.
- Permanent Agent Task closure is a separate future event or explicit operator action.
- Build a provider conversation reconstruction module as a deep CLI module. Its public interface should accept a Transcript Session id and return a provider-normalized input sequence or a precise resume error.
- Build a transcript session query module as a deep CLI module. Its public interface should support listing sessions by project, loading events by session, and locating pending action state.
- Build an event schema module as a deep CLI module. Its public interface should produce Warp-compatible compact events and canonical persisted events from the same domain event.
- Build a permission classifier as a deep CLI module. Its public interface should classify tool calls as read-only or permissioned before execution.
- Initial read-only tools are `read`, `glob`, `grep`, `ls`, `session_search`, todo reads, and memory reads.
- Initial permissioned tools are `write`, `edit`, `multiedit`, `bash`, todo writes, and memory writes.
- Unknown or unclassified tools require permission by default.
- Build a permission coordinator as a deep CLI module. Its public interface should create permission requests, wait for decisions, persist pending state, consume replies, and return approve/deny decisions to the Agent Tool Loop.
- Emacs delivers Permission Decisions through a Permission Reply Command, not through process stdin.
- The initial reply command shape is a CLI operation that accepts a request id and an approve or deny decision.
- Pending permission requests do not auto-approve or auto-deny on a timer.
- If the live agent process exits while permission is pending, the Transcript Session remains blocked and Agent Resume can continue from that blocked state.
- The Agent Tool Loop emits `permission_request`, waits for a Permission Decision, emits `permission_replied`, and continues only after approval or denial.
- Agent questions use `question_asked` and are answered as normal Agent Resume prompts. They do not use the Permission Reply Command.
- Build an Emacs Agent Event Ingestion module as a shared path for notebook process filters and later Terminal Surface OSC ingestion.
- Notebook-launched agent processes are the required event source for Agent Notebook v1.
- Terminal Surface OSC ingestion through EAT/tmux is a compatibility spike for terminal-rendered fallback sessions and should plug into the shared ingestion path later.
- Build an Emacs notebook projection module. Its public interface should open a notebook by session id, create a new notebook task, render cells from transcript events, and submit the current draft cell.
- Build an Emacs notebook renderer that maps events into Notebook Cells. It should support Draft, Running, Complete, and Blocked cell states.
- A Draft Notebook Cell is the only editable region. Running, Complete, and Blocked cells are read-only.
- Submitting a Draft Notebook Cell uses one non-interactive Agent Resume invocation for v1.
- The notebook streams live process output/events into the Running Notebook Cell, then reconciles the completed cell from the Canonical Event Log.
- Prompt and response text render expanded by default.
- Tool activity renders collapsed by default, using tool name and concise preview as the visible summary.
- Permission requests and questions render prominently because they require operator action.
- Build an Emacs Agent Action Transient module for question and permission events.
- Permission transients include approve, deny, open notebook, and dismiss/review actions.
- Question transients include answer, open notebook, and dismiss/review actions.
- Build an Emacs Agent Action Queue as the first Agent Inbox slice.
- The Agent Action Queue lists pending permission requests, questions, completed cells, and errored sessions.
- Selecting an Agent Action Queue item opens the relevant Project Workspace and Agent Notebook.
- Selecting an actionable item also opens the Agent Action Transient.
- Build a minimal Emacs-local notification presenter. It should use an in-Emacs popup when available and fall back to an Emacs message plus an Agent Action Queue entry.
- Do not use desktop notifications in this slice.
- Mark Agent Notifications read when the specific Transcript Session is opened.
- Keep the Project Dashboard out of this slice. The Agent Action Queue is not a project-management dashboard.
- Keep org export for Agent Skills out of this slice. The Workflow Agent CLI may consume exported skills later, but this PRD does not build the Skill Export Backend.
- Keep Chat Completions and Alibaba Coding Plan adapters out of this slice. Resume is built around the Responses Provider Profile first.

## Testing Decisions

- Good tests should cover external behavior and domain contracts, not private helper shape.
- The Agent Resume reconstruction tests should build sample transcript events, load a Transcript Session, and assert the reconstructed provider input sequence. These tests should not call the network.
- The resume error tests should cover missing sessions, malformed Provider Conversation Items, unsupported provider items, and attempts to resume a session that cannot be reconstructed exactly.
- The title/session tests should cover explicit titles, derived titles, session id stability, and title metadata appearing in canonical events.
- The Canonical Event Log tests should assert that persisted events include the resume-critical Provider Conversation Items while compact live OSC events do not include large provider payloads.
- The Transcript Index tests should assert lookup by session id, project, event type, pending action state, and rebuild from the Canonical Event Log.
- The Warp event compatibility tests should assert that emitted v1 events use the expected event names and fields, and that extra fields remain optional/ignorable.
- The permission classifier tests should cover read-only tools, permissioned tools, todo read/write distinctions, memory read/write distinctions, aliases, and unknown tool defaulting to permissioned.
- The permission coordinator tests should cover creating a request, persisting pending state, approving, denying, duplicate replies, stale request ids, and blocked state surviving process restart boundaries.
- The Agent Tool Loop tests should cover permissioned tool calls pausing before execution, approved calls executing, denied calls returning a denial result to the model, and `permission_replied` events being recorded.
- The question handling tests should cover `question_asked` becoming a blocked/actionable state and answer submission using Agent Resume rather than Permission Reply Command.
- The Interactive Agent Resume tests should cover promptless resume reading line-oriented input without becoming a full-screen TUI.
- The CLI command tests should cover `run`, `run --title`, `resume SESSION_ID PROMPT`, promptless `resume SESSION_ID`, and permission reply commands.
- The Emacs event ingestion tests should feed valid and invalid Warp-compatible events into the shared parser and assert notebook/action/notification updates.
- The Emacs notebook rendering tests should render transcript fixtures into Draft, Running, Complete, and Blocked Notebook Cells.
- The Emacs notebook editability tests should assert that only the Draft Notebook Cell is editable and prior cells are read-only.
- The Emacs notebook submission tests should assert that submitting the draft invokes Agent Resume with the correct session id and prompt.
- The Emacs notebook reconciliation tests should simulate streamed output followed by transcript reload and assert the completed cell reflects canonical transcript content.
- The Emacs collapsed-tool tests should assert that tool activity is present but folded/collapsed by default.
- The Agent Action Transient tests should verify that permission and question events open the right transient actions and call the correct CLI operations.
- The Agent Action Queue tests should verify listing, filtering, activation, and read-state behavior for pending, unread, completed, and errored events.
- The notification tests should verify that Emacs-local notification activation opens the Project Workspace, Agent Notebook, and action transient when applicable.
- Existing CLI tests use Alcotest and can be extended for CLI/domain behavior.
- Existing Emacs package tests in the repo are prior art for ERT-style tests of command behavior and buffer state.
- Manual validation should include creating a new notebook, submitting multiple cells, resuming an old session, approving and denying a permission request, answering a question, recovering a pending action from the queue, and confirming no desktop notifications are produced.

## Out of Scope

- Building a full-screen terminal UI for the Workflow Agent CLI.
- Making terminal-rendered agent sessions the primary daily workflow.
- Replacing the Canonical Event Log with per-session canonical files.
- Summarizing or compressing Provider Conversation Items for resume.
- Chat Completions provider support.
- Alibaba Coding Plan provider support.
- Remote Execution Handoff implementation.
- Org export for Agent Skills.
- Project Dashboard from org files and agent-produced PRDs.
- Weekly summary generation.
- Semantic Agent Memory, embeddings, graph retrieval, clustering, sidecar verification, or async memory agents.
- Browser automation, web search, MCP, subagents, batch execution, ambient work, self-development, side panels, and Gmail Integration Tools.
- Desktop notification integration.
- Full autonomy presets or a rich policy editor for Agent Permission Flow.
- Background bash jobs, stdin interaction for bash tools, or long-running tool process supervision.
- Strict per-tab buffer isolation.
- Making Agent Notebook buffers canonical saved org documents.
- Importing existing Codex, jcode, or other external agent transcripts.

## Further Notes

This PRD supersedes the earlier decision that Agent Permission Flow was deferred.
The new accepted direction is to implement a minimal Emacs Transient-based
permission flow in this slice.

The Agent Notebook deliberately uses Transcript Session identity rather than
tmux session identity. tmux and EAT remain valuable for Durable Sessions and
terminal fallback, but the daily agent workflow is the Agent Notebook.

The Universal Agent Support Channel remains the live UI signal path, not the
canonical persistence layer. The Workflow Agent CLI writes the Canonical Event
Log; Emacs consumes live events for UI updates and reads the transcript store to
render/reconcile notebooks.

This PRD should be broken into implementation issues along the three vertical
slices: Resume Core, Agent Notebook, and Actions. Resume Core should land first
because the notebook and permission flow depend on stable Transcript Session
identity, exact provider replay, and pending action persistence.
