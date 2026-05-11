# Agent Memory Starts Lightweight

The Workflow Agent CLI will expose a jcode-shaped `memory` tool in the first tool surface, but the initial Agent Memory implementation will be a lightweight durable local store of explicit facts, preferences, and procedures. jcode's richer vector, graph, clustering, sidecar-verification, and async retrieval design is deferred until the transcript store and basic tool loop are reliable.
