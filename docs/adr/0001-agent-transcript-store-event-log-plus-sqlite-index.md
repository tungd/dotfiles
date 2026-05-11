# Agent Transcript Store Uses JSONL Event Log Plus SQLite Index

The Agent Transcript Store records CLI Agent Events globally across projects. We
will write append-only JSONL as the canonical event log and maintain SQLite as a
rebuildable transcript index, rather than making SQLite the only source of truth.
This preserves an inspectable, recoverable event history while still supporting
fast project, session, time-range, and event-type queries for summaries and
agent context.
