# Agent Resume Replays Provider Conversation Items

Agent Resume will reconstruct the next provider request from stored Provider
Conversation Items for the target Transcript Session, rather than from rendered
rendered text or a summary. This makes the Canonical Event Log resumable as well
as inspectable; transcript summarization and compression are deferred until exact
resume works reliably.
