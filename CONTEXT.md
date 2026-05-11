# Dotfiles Command Workspace

This context defines the language for replacing a standalone terminal app with
an Emacs-centered command workspace.

## Language

**Command Workspace**:
The primary place where tabs, panes, shells, long-running sessions, remote work,
and coding-agent interactions are managed.
_Avoid_: daily terminal, terminal app

**Terminal Surface**:
An Emacs buffer that renders an interactive terminal program for direct human
use.
_Avoid_: shell, pane

**Terminal Backend**:
The Emacs terminal emulator implementation used by a Terminal Surface.
_Avoid_: command workspace

**Interactive Shell**:
A short-lived command-line session used directly by the operator.
_Avoid_: durable session, agent session

**Durable Session**:
A long-lived command-line session that survives Emacs buffer and window churn.
_Avoid_: terminal buffer

**Project Session**:
A durable command-line session whose default identity and working directory come
from the current project.
_Avoid_: tab, pane, anonymous shell

**Project Terminal**:
The command workflow that opens or creates the current project's default
interactive Project Session.
_Avoid_: open shell, new tab

**Project Workspace**:
An Emacs tab-bar tab dedicated to one project and its working buffers,
windows, and Project Terminal.
_Avoid_: Warp tab, project session

**Project Dashboard**:
A future project-management view assembled from project org files and
agent-produced PRDs.
_Avoid_: startup screen, terminal tab

**Agent Session**:
A long-running coding-agent process that may be observed or controlled from
Emacs without relying on full-screen terminal rendering as the primary UI.
_Avoid_: shell, terminal

**Transcript Session**:
A durable grouping of CLI Agent Events that remains after its live agent process
or terminal surface is gone.
_Avoid_: tmux session, process

**Agent Resume**:
A Workflow Agent CLI operation that continues an Agent Task from an existing
Transcript Session, optionally with a new operator prompt.
_Avoid_: replay, new run, Emacs-side context reconstruction

**Interactive Agent Resume**:
A line-oriented Agent Resume mode that reads follow-up prompts from standard
input without becoming a full-screen terminal UI.
_Avoid_: TUI session, terminal-rendered agent

**Agent Task**:
A coherent unit of agent work with an explicit title and one Transcript Session.
_Avoid_: prompt, chat

**Task Anchor**:
An optional link from an Agent Task to external planning context such as an org
heading, PRD, issue, or pull request.
_Avoid_: required task source

**Agent Cell Session**:
A code-cells workflow where prompts are sent to a coding agent and results are
rendered back into an editable Emacs buffer.
_Avoid_: chat buffer, terminal transcript

**Agent Notebook**:
An Emacs buffer for one Transcript Session where past prompt/result cells are
read-only, the latest prompt cell is editable, and submitting it continues the
same Agent Task.
_Avoid_: chat buffer, terminal transcript, read-only transcript view

**Notebook Projection**:
A disposable Agent Notebook rendering derived from the Agent Transcript Store.
_Avoid_: source document, canonical org file

**Notebook Cell**:
One operator prompt and the agent continuation that follows until the task
stops, becomes idle, or asks for more input.
_Avoid_: provider turn, tool call, terminal block

**Draft Notebook Cell**:
The single editable latest Notebook Cell whose prompt has not been submitted.
_Avoid_: scratch buffer, unsaved transcript

**Running Notebook Cell**:
A Notebook Cell whose prompt has been submitted and whose agent continuation is
still producing events.
_Avoid_: draft prompt, terminal command

**Complete Notebook Cell**:
A read-only Notebook Cell whose agent continuation has emitted `stop`.
_Avoid_: closed task, archived transcript

**Blocked Notebook Cell**:
A read-only Notebook Cell whose agent continuation is waiting on a question or
permission event.
_Avoid_: editable prompt cell

**Transcript View**:
A read-only code-cells-style Emacs buffer that renders a Transcript Session.
_Avoid_: live prompt buffer, terminal scrollback

**Workflow Agent CLI**:
A future coding-agent command-line program designed around this command
workspace instead of around a full-screen terminal UI.
_Avoid_: smolagent, td-acp, terminal-rendered agent

**Agent Source Project**:
The standalone source repository/package that implements the Workflow Agent CLI.
_Avoid_: dotfiles module, Emacs package

**Headless Agent Runtime**:
The first Workflow Agent CLI runtime mode, which owns the Agent Tool Loop but
does not own a full-screen interface.
_Avoid_: TUI agent, sidebar app

**Agent Runtime Handler**:
The OCaml effect handler that supplies concrete filesystem, HTTP, process,
clock, event, and concurrency capabilities to the Workflow Agent CLI core.
_Avoid_: global IO, hard-coded runtime

**Model Provider Profile**:
A named configuration for a model backend, including endpoint, authentication
source, model identity, and capability contract.
_Avoid_: hard-coded provider, backend

**OpenAI-Compatible Provider Profile**:
A Model Provider Profile whose transport and responses follow OpenAI-style API
contracts while allowing non-OpenAI endpoints and models.
_Avoid_: OpenAI clone, Alibaba-only backend

**Responses Provider Profile**:
A Model Provider Profile that exposes OpenAI Responses-style events to the Agent
Tool Loop.
_Avoid_: chat-completions provider

**Provider Conversation Item**:
A provider-normalized message, reasoning item, tool call, or tool result needed
to reconstruct an Agent Tool Loop continuation exactly.
_Avoid_: rendered transcript cell, summary

**Codex OAuth Credential**:
An OpenAI/Codex account credential source used by a Model Provider Profile.
_Avoid_: API key, terminal login

**Codex Auth Source**:
A local file that can provide Codex OAuth credentials to the Workflow Agent CLI.
_Avoid_: account manager, provider profile

**Agent Tool Loop**:
The Workflow Agent CLI control loop that alternates model responses, tool
execution, and operator input until the Agent Task is complete.
_Avoid_: chat completion, terminal UI loop

**Agent Permission Flow**:
The operator approval workflow for tool calls that mutate state or execute
commands.
_Avoid_: tool execution, transcript logging

**Permission Decision**:
An operator approve or deny response to an Agent Permission Flow request.
_Avoid_: tool result, notification dismissal

**Permission Reply Command**:
A Workflow Agent CLI command used by Emacs to deliver a Permission Decision for
a pending permission request.
_Avoid_: stdin approval, terminal keystroke

**Agent Tool Surface**:
The set of native tools and tool-call conventions exposed by the Workflow Agent
CLI to the model.
_Avoid_: ad hoc command list, shell wrapper

**Core Coding Tool**:
A first-slice Agent Tool Surface tool needed for ordinary repository inspection,
editing, shell execution, todo tracking, skills, memory, or transcript search.
_Avoid_: integration tool, sideview tool

**Read-Only Agent Tool**:
A Core Coding Tool that can inspect state without changing files, memory, todos,
processes, or external systems.
_Avoid_: safe tool

**Permissioned Agent Tool**:
A Core Coding Tool that may change files, memory, todos, processes, or external
systems and therefore requires Agent Permission Flow.
_Avoid_: dangerous tool

**Integration Tool**:
A later Agent Tool Surface tool that connects the Workflow Agent CLI to browsers,
web search, MCP servers, background swarms, self-development workflows, or other
external systems.
_Avoid_: core coding tool

**Agent Skill**:
A local instruction or workflow package exposed to the Workflow Agent CLI as a
reusable agent capability.
_Avoid_: plugin, prompt snippet

**Skill Source File**:
The operator-owned org file that is the canonical source for Agent Skills.
_Avoid_: generated skill directory, imported skill copy

**Skill Export Backend**:
An org export backend that materializes Agent Skills from the Skill Source File
into tool-specific skill artifacts.
_Avoid_: skill importer, sync script

**Agent Memory**:
Durable cross-session knowledge the Workflow Agent CLI can retrieve to guide
future Agent Tasks.
_Avoid_: transcript, chat history

**Lightweight Agent Memory**:
The first Agent Memory implementation: a durable local store of explicit facts,
preferences, and procedures without embeddings, graph traversal, or sidecar
verification.
_Avoid_: semantic memory graph, transcript store

**Remote Execution Handoff**:
The transfer of an Agent Task from a local Agent Session to a remote runner while
preserving task and transcript identity.
_Avoid_: SSH session, remote shell

**CLI Agent Event**:
A structured event emitted by a Workflow Agent CLI to describe session state,
prompts, tool activity, permission flow, questions, and completion.
_Avoid_: screen scrape, terminal spinner

**CLI Agent Event Transport**:
The Warp-compatible Universal Agent Support channel that carries CLI Agent
Events over OSC 777 from the agent process to Emacs and other compatible
terminals.
_Avoid_: terminal transcript, Emacs-only event pipe

**Universal Agent Support Channel**:
The OSC 777 `warp://cli-agent` channel whose JSON body contains versioned CLI
Agent Events.
_Avoid_: custom notification protocol, desktop notification channel

**Agent Event Ingestion**:
The Emacs path that validates CLI Agent Events and updates notebooks,
notifications, and the Agent Action Queue.
_Avoid_: terminal scraping, notebook-only parser

**Agent Transcript Store**:
A durable record of CLI Agent Events used for session recovery, weekly review,
summaries, and future agent context.
_Avoid_: tmux scrollback, terminal log

**Agent Notification**:
An Emacs-owned notification derived from a validated CLI Agent Event.
_Avoid_: generic terminal notification

**Agent Action Transient**:
An Emacs transient menu opened from a question or permission CLI Agent Event to
collect an operator decision.
_Avoid_: minibuffer prompt, terminal approval UI

**Agent Inbox**:
A persistent Emacs buffer listing Agent Notifications and agent sessions that
need review.
_Avoid_: desktop notification center

**Agent Action Queue**:
The first Agent Inbox slice: a list of pending or unread agent events that need
operator review.
_Avoid_: project dashboard, weekly summary

**Transcript Index**:
A SQLite-backed index over the Agent Transcript Store for project, session, time,
and event queries.
_Avoid_: grep over logs

**Canonical Event Log**:
The append-only JSONL source of truth for CLI Agent Events.
_Avoid_: database dump, tmux capture

**Terminal-Rendered Agent Session**:
An agent session whose primary interface is a full terminal transcript controlled
by the agent process.
_Avoid_: agent workspace

## Relationships

- The **Command Workspace** owns **Terminal Surfaces**, **Durable Sessions**, and
  **Agent Sessions**.
- The next **Agent Notebook** implementation is split into three vertical
  slices: **Agent Resume** core, **Agent Notebook** projection, and Emacs
  action handling.
- A **Project Workspace** is the primary replacement for a Warp tab.
- A **Project Workspace** contains ordinary Emacs windows, which replace Warp
  panes.
- A **Project Workspace** is unique per project root; opening a project switches
  to its existing workspace when present and creates one only when missing.
- A newly created **Project Workspace** opens on Magit status; a **Project
  Dashboard** may replace this initial view later.
- **Project Workspace** scoping is layout-first: project buffers and Project
  Terminal buffers are named and displayed per workspace, without strict global
  buffer isolation unless ordinary buffer switching becomes noisy.
- The concrete emulator backing a **Terminal Surface** is replaceable; it is
  selected by responsiveness, Emacs integration, and compatibility.
- The **Terminal Backend** choice gates the first implementation slice and may
  affect the later Workflow Agent CLI integration shape.
- A **Terminal Backend** is acceptable only if it handles tmux attach/detach,
  resize, fast output, common interactive TUIs, copy/search/scroll behavior,
  OSC 777 passthrough visibility, and long-running output without serious
  cursor or scroll corruption.
- An **Interactive Shell** normally runs inside a **Project Session** rendered by
  a **Terminal Surface**.
- A **Durable Session** may host an **Interactive Shell** or an **Agent Session**.
- A **Project Session** is a **Durable Session**.
- A **Project Session** normally uses the project basename as its identity, with
  a short stable suffix from the full project root only when needed to avoid a
  collision.
- A **Project Session** is created lazily by an explicit command and reused after
  creation; switching projects does not create sessions by itself.
- A **Project Terminal** opens the current project's **Project Session** and
  creates it first when it does not exist.
- Each project has one default **Project Terminal** for ordinary shell work.
- An **Agent Session** uses a role-prefixed **Project Session** identity such as
  `codex-dotfiles` or `claude-dotfiles`.
- An **Agent Cell Session** controls an **Agent Session** without treating the
  terminal transcript as the main interaction model.
- An **Agent Notebook** is the preferred **Agent Cell Session** shape for daily
  use.
- An **Agent Notebook** renders immutable prior cells from a **Transcript
  Session** and exposes one editable latest prompt cell.
- An **Agent Notebook** is a **Notebook Projection**, not an additional source of
  truth.
- An **Agent Notebook** can be opened for an existing **Transcript Session** or
  created by starting a new **Agent Task**.
- An **Agent Notebook** is rendered as **Notebook Cells**.
- A **Notebook Cell** groups all provider responses, tool calls, and output that
  result from one operator prompt.
- **Notebook Cells** show prompts and response text by default, while tool
  activity is collapsed unless expanded.
- Permission requests and agent questions are shown prominently because they
  require operator action.
- An **Agent Notebook** has at most one **Draft Notebook Cell**.
- Submitting a **Draft Notebook Cell** turns it into a **Running Notebook Cell**.
- A `stop` event turns a **Running Notebook Cell** into a **Complete Notebook
  Cell**.
- A question or permission event turns a **Running Notebook Cell** into a
  **Blocked Notebook Cell**.
- An `idle_prompt` event after a **Complete Notebook Cell** creates the next
  **Draft Notebook Cell**.
- Submitting the editable prompt cell in an **Agent Notebook** continues the
  same **Agent Task** rather than creating a new task.
- A **Transcript View** remains read-only; editable input belongs to an **Agent
  Notebook**.
- **Agent Resume** is owned by the **Workflow Agent CLI**, not reconstructed in
  Emacs.
- An **Agent Notebook** submits new prompts through **Agent Resume** by passing
  the **Transcript Session** identity and, when present, the new operator
  prompt.
- The first **Agent Notebook** submits each editable cell through one
  non-interactive **Agent Resume** invocation.
- A new agent run creates a new **Agent Task** and **Transcript Session**;
  **Agent Resume** appends to an existing **Transcript Session**.
- Creating a new **Agent Notebook** starts a new **Agent Task** and binds the
  notebook to the emitted **Transcript Session** identity.
- When **Agent Resume** receives no prompt argument, it starts an **Interactive
  Agent Resume** for that **Transcript Session**.
- **Interactive Agent Resume** uses line-oriented standard input and output; it
  is not a **Terminal-Rendered Agent Session**.
- A **Terminal-Rendered Agent Session** is a fallback for inspection or rescue,
  not the preferred daily agent workflow.
- Existing `drepl-smolagent` and `td-acp` integrations are exploration artifacts,
  not foundations for the **Workflow Agent CLI**.
- Existing local agent experiments may be removed during cleanup, including
  terminal-specific wrappers and prototype parsers.
- A live **Agent Session** and a **Transcript Session** are separate lifecycles:
  live process TTL affects runtime cleanup, while transcript lifetime is
  controlled by archive/delete policy.
- Each **Agent Task** owns one **Transcript Session**; follow-up prompts for the
  same work stay in that session until the task is done.
- **Agent Tasks** are created explicitly and named, rather than auto-created for
  every prompt or collapsed into one endless project session.
- An **Agent Task** has a title at creation time; when no title is supplied, the
  title may be derived from the first operator prompt.
- The **Agent Task** title is stored in **CLI Agent Events** so selectors,
  notebooks, summaries, and inbox views can use it.
- An **Agent Task** may later gain a **Task Anchor**.
- A **Workflow Agent CLI** remains a first-class command-line program and emits
  **CLI Agent Events** for the Command Workspace to react to.
- The **Workflow Agent CLI** is implemented in an **Agent Source Project** outside
  this dotfiles repository.
- This dotfiles repository configures, launches, and integrates with the
  **Workflow Agent CLI**, but does not own its source code.
- The first **Workflow Agent CLI** implementation is a **Headless Agent
  Runtime**.
- The **Headless Agent Runtime** separates core control flow from side effects
  through **Agent Runtime Handlers**.
- **Agent Runtime Handlers** allow the same agent loop to run locally, under
  tests, or under a future remote execution handoff.
- A **Headless Agent Runtime** writes human-readable terminal output only as a
  convenience; **CLI Agent Events** and transcript records are the primary
  interface.
- A **Workflow Agent CLI** is driven by an **Agent Tool Loop**, not by a
  full-screen terminal UI.
- The first **Agent Tool Surface** should follow jcode's native coding-agent
  conventions closely, rather than inventing a minimal custom set of tool names.
- The **Agent Tool Surface** may be implemented in slices, but tool names,
  schemas, aliasing, and transcript events should remain compatible with the
  jcode-shaped model where practical.
- The first **Agent Tool Surface** slice consists of **Core Coding Tools**:
  `read`, `write`, `edit`, `multiedit`, `glob`, `grep`, `ls`, `bash`, `todo`,
  `memory`, and `session_search`.
- The first **Agent Tool Surface** slice preserves jcode-style aliases such as
  `file_read` to `read`, `file_write` to `write`, `file_edit` to `edit`,
  `file_glob` to `glob`, `file_grep` to `grep`, `shell_exec` to `bash`, and
  `todo_read`/`todo_write` to `todo`.
- **Integration Tools** such as browser automation, web fetch/search, MCP,
  subagents, batch execution, ambient work, self-development, side panels, and
  Gmail are deferred.
- **Agent Permission Flow** is handled through **Agent Action Transients** in
  Emacs.
- The **Workflow Agent CLI** emits `permission_request`, waits for a
  **Permission Decision**, records `permission_replied`, and only then continues
  the **Agent Tool Loop**.
- Emacs delivers **Permission Decisions** through a **Permission Reply Command**,
  not through the agent process terminal stdin.
- Read-only tools may run without **Agent Permission Flow**; mutating tools and
  command execution require a **Permission Decision** in the first permission
  slice.
- A pending **Agent Permission Flow** blocks the **Agent Task** until answered;
  v1 does not auto-approve or auto-deny on a timer.
- If the live agent process exits while permission is pending, the **Transcript
  Session** remains blocked and can be continued through **Agent Resume**.
- **Read-Only Agent Tools** initially include `read`, `glob`, `grep`, `ls`,
  `session_search`, todo reads, and memory reads.
- **Permissioned Agent Tools** initially include `write`, `edit`, `multiedit`,
  `bash`, todo writes, and memory writes.
- Unknown or unclassified tools are treated as **Permissioned Agent Tools**.
- The first `bash` **Core Coding Tool** executes foreground commands with a
  bounded timeout and captured stdout, stderr, and exit status.
- Background tasks, stdin interaction, notifications, and tool-level process
  supervision are deferred for `bash`.
- The first `session_search` **Core Coding Tool** searches only the Workflow
  Agent CLI's own **Agent Transcript Store** and **Transcript Index**.
- Importing or searching existing Codex, jcode, or other external transcripts is
  deferred.
- A **Model Provider Profile** keeps provider configuration outside the
  **Agent Tool Loop**.
- The first **Model Provider Profile** supported by the **Workflow Agent CLI** is
  a **Responses Provider Profile**.
- The **Agent Tool Loop** is shaped around **Responses Provider Profile** events
  before chat-completions adapters are introduced.
- **Agent Resume** reconstructs provider input from stored **Provider
  Conversation Items** for the target **Transcript Session**.
- The first **Agent Resume** implementation replays exact **Provider
  Conversation Items** rather than summarizing or compressing history.
- If exact resume cannot be reconstructed, the **Workflow Agent CLI** reports a
  resume error instead of silently creating a new **Agent Task**.
- An **OpenAI-Compatible Provider Profile** can represent non-OpenAI services
  such as the existing Alibaba Coding Plan configuration, but Alibaba Coding Plan
  support is deferred until after the Responses path works.
- A **Codex OAuth Credential** is one credential source for a **Model Provider
  Profile** and is a core v1 requirement.
- The first **Workflow Agent CLI** keeps one active **Codex OAuth Credential**,
  not a set of labeled OpenAI accounts.
- The first **Workflow Agent CLI** may read the existing Codex CLI auth file as
  a trusted **Codex Auth Source** without an interactive consent prompt.
- A **Codex Auth Source** is read-only unless the operator explicitly runs the
  Workflow Agent CLI's own login command.
- The first credential lookup order is the Workflow Agent CLI's own auth file,
  then the existing Codex CLI auth file, then API-key fallback.
- API keys may exist as fallback or test credentials, but they do not define the
  first authentication path for the **Workflow Agent CLI**.
- **Agent Skills** shape the behavior and tool choices of the **Workflow Agent
  CLI** without requiring terminal rendering.
- **Agent Skills** are authored in the **Skill Source File** and materialized by
  a **Skill Export Backend**.
- The **Workflow Agent CLI** consumes exported **Agent Skills**; it does not own
  skill authoring, importing, syncing, or distribution.
- `skill_manage` is deferred until the **Skill Export Backend** exists.
- **Agent Memory** may be seeded from the **Agent Transcript Store**, but it is
  not the same thing as the transcript.
- The first **Agent Memory** implementation is **Lightweight Agent Memory** while
  preserving a jcode-shaped `memory` tool interface.
- Embeddings, semantic graph retrieval, sidecar verification, clustering, and
  async memory agents are deferred until after the transcript store and basic
  tool loop are reliable.
- A **Remote Execution Handoff** keeps the same **Agent Task** and **Transcript
  Session** identity across local and remote execution.
- The **CLI Agent Event Transport** follows Warp's structured OSC 777 shape:
  title `warp://cli-agent`, JSON body, protocol version, agent identity, session
  identity, project context, and event-specific fields.
- The **CLI Agent Event Transport** is the **Universal Agent Support Channel**,
  not an Emacs-specific side channel.
- The **Workflow Agent CLI** emits Warp v1-compatible **CLI Agent Events** and
  may include extra fields for Emacs and transcript reconstruction.
- Extra **CLI Agent Event** fields must be ignorable by Warp-compatible
  terminals.
- The live **Universal Agent Support Channel** carries compact UI-oriented
  **CLI Agent Events**.
- Full **Provider Conversation Items** are persisted in the **Canonical Event
  Log**, not sent wholesale through OSC 777.
- **Agent Event Ingestion** accepts events from notebook-launched agent
  processes and from Terminal Surfaces that pass through the **Universal Agent
  Support Channel**.
- Notebook-launched agent processes are the required **Agent Event Ingestion**
  source for the first **Agent Notebook** slice.
- Notebook-launched **Agent Sessions** may stream output into the active
  **Running Notebook Cell** as live UI feedback.
- A completed **Notebook Cell** is reconciled from the **Canonical Event Log** so
  the transcript remains authoritative over streamed UI text.
- Terminal Surface OSC ingestion through EAT/tmux is a compatibility spike for
  terminal-rendered fallback sessions, not a blocker for the first **Agent
  Notebook**.
- **Agent Event Ingestion** is live UI plumbing; the **Canonical Event Log**
  remains written by the **Workflow Agent CLI**.
- **CLI Agent Events** are expected to pass through tmux to Emacs because tmux
  provides session durability while Emacs owns event handling and persistence.
- Important **CLI Agent Events** include session start, prompt submitted, tool
  completed, task stopped, permission requested, permission replied, question
  asked, and idle prompt.
- `idle_prompt` is carried as a **CLI Agent Event** on the **Universal Agent
  Support Channel**.
- `stop` ends the current **Notebook Cell** continuation; it does not by itself
  mean the **Agent Task** is permanently closed.
- `idle_prompt` means the same **Transcript Session** can accept another
  operator prompt.
- Permanent **Agent Task** closure is a separate future event or explicit
  operator action.
- `question_asked` and `permission_request` events are surfaced through an
  **Agent Action Transient**.
- Answering a `question_asked` event submits a normal **Agent Resume** prompt for
  the same **Transcript Session**.
- Permission requested and permission replied events carry **Agent Permission
  Flow** over the **Universal Agent Support Channel**.
- **CLI Agent Events** are both live UI signals and source records for the
  **Agent Transcript Store**.
- **Agent Notifications** are derived from validated **CLI Agent Events** rather
  than from arbitrary terminal notification escape sequences.
- **Agent Notifications** are Emacs-local by default, shown inside Emacs rather
  than as desktop notifications.
- The first **Agent Notification** implementation uses a minimal Emacs-local
  popup when available and falls back to an Emacs message plus **Agent Action
  Queue** entry.
- Activating an **Agent Notification** navigates to the relevant **Project
  Workspace** and **Transcript Session**.
- The **Agent Inbox** is the persistent place to review missed, unread, blocked,
  errored, and completed agent activity.
- The first **Agent Inbox** implementation is an **Agent Action Queue**.
- The **Agent Action Queue** lists pending permission requests, agent questions,
  completed cells, and errored sessions.
- Selecting an **Agent Action Queue** item opens the relevant **Agent Notebook**
  and, when applicable, the **Agent Action Transient**.
- An **Agent Notification** is marked read when its specific **Transcript
  Session** is opened.
- The **Agent Transcript Store** is global and canonical across projects.
- The **Agent Transcript Store** writes a **Canonical Event Log** first.
- **Notebook Projections** are rebuilt from the **Agent Transcript Store**; only
  the current editable prompt cell is local unsaved state.
- The **Transcript Index** is rebuildable from the **Canonical Event Log** and
  makes the global store queryable by project, session, time range, and event
  type.
- **Agent Resume** reads the global **Canonical Event Log** through the
  **Transcript Index** rather than requiring per-session log files.
- Every stored **CLI Agent Event** includes event id, timestamp, schema version,
  agent identity, session id, project root, project name, event type, and
  event-specific payload.
- The **Agent Transcript Store** supports recovering from short-lived UI
  sessions, summarizing weekly work, and feeding prior work back into agents.

## Example Dialogue

> **Dev:** "Are we replacing Warp with another terminal emulator?"
> **Domain expert:** "No. Emacs is the **Command Workspace**. Terminal rendering
> is only one **Terminal Surface** inside it."

## Flagged Ambiguities

- "daily terminal" was used to mean both a terminal emulator and the whole
  command workflow; resolved: the canonical term is **Command Workspace**.
- "agent session" previously included both full-terminal agent UIs and
  code-cells workflows; resolved: **Agent Cell Session** is the preferred daily
  model, while **Terminal-Rendered Agent Session** is fallback.
- "notebook" means **Agent Notebook**, not a general editable document or a
  read-only **Transcript View**.
- `drepl-smolagent` and `td-acp` were both exploratory agent integrations;
  resolved: neither defines the target agent architecture.
- `codex.el`, `claudecode.el`, `vterm-agent.el`, `drepl-smolagent.*`, and
  `td-acp*` are cleanup candidates rather than compatibility constraints.
- "OpenAI-style API responses" means an **OpenAI-Compatible Provider Profile**,
  not a hard dependency on OpenAI-hosted models.
- "Alibaba Coding Plan support" means a later provider adapter, not a constraint
  on the first Responses-focused agent core.
- "session_search" means search over this Command Workspace's transcript store
  first, not a general importer for every external agent transcript.
