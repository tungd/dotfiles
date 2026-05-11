# Agent Permission Flow Uses Emacs Transient

Agent Permission Flow is now in scope for the Agent Notebook slice and will use
an Emacs Agent Action Transient rather than terminal prompts or desktop
notifications. The Workflow Agent CLI emits `permission_request`, waits for a
`permission_replied` decision, and continues the Agent Tool Loop only after
approval; this keeps permission handling inside the Command Workspace while
preserving the Warp-compatible Universal Agent Support Channel.
