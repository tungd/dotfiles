# Agent Tool Surface Follows jcode

The Workflow Agent CLI should follow jcode's native coding-agent tool surface closely instead of starting from a tiny custom tool set. This means preserving the broad shape of jcode's tool registry, tool-call aliases, and coding workflow expectations while still implementing the surface in focused slices and excluding UI-specific pieces that conflict with the headless Emacs-owned runtime.
