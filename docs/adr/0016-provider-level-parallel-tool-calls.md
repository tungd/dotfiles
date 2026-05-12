# Provider-Level Parallel Tool Calls

The Workflow Agent CLI will pursue jcode-style parallel tool calls through the provider contract (`parallel_tool_calls=true`) rather than by adding a model-facing `batch` tool. A Parallel Tool Call Scheduler inside the Agent Tool Loop will preserve canonical event recording, call-id ordering, and Agent Permission Flow.

The scheduler should follow jcode's phase model: consecutive compatible auto-executable read-only calls may run in a `Parallel(ReadOnlyLocalContext)` phase, while permissioned, mutating, interactive, or otherwise incompatible calls run as `Serial` barriers. A parallel phase must drain before later calls start, and finished results are sorted back into original provider tool-call order before the follow-up model request is built.

This is the next implementation slice before web fetch/search, skill discovery, supervised long-running `bash`, and Ambient Agent Run/Execution support.
