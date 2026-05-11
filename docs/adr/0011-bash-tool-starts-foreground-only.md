# Bash Tool Starts Foreground Only

The first `bash` tool will execute foreground commands with bounded timeout and captured stdout, stderr, and exit status. Background tasks, stdin interaction, notifications, and long-running process supervision are deferred because terminal durability is already handled by tmux/EAT, while the agent tool path first needs reliable Responses integration, CLI Agent Events, and transcript persistence.
