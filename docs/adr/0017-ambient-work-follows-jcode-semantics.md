# Ambient Work Follows jcode Semantics

Ambient/background work in the Workflow Agent CLI will follow jcode-shaped runtime semantics. The stable identity is an ambient run/task, while each startup, retry, or follow-up is an execution with its own session transport and runtime state. Ambient work must still emit canonical CLI Agent Events and remain inspectable and controllable from the standalone CLI and later Emacs surfaces.
