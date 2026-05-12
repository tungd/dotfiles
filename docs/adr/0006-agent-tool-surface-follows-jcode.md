# Agent Tool Surface Follows jcode

The Workflow Agent CLI should follow jcode's native coding-agent tool surface closely instead of starting from a tiny custom tool set. This means preserving the broad shape of jcode's tool registry and coding workflow expectations while still implementing the surface in focused slices and excluding UI-specific pieces that conflict with the headless Emacs-owned runtime. Tool names are canonical in `td-agent`; jcode-style alias spellings are not part of compatibility.
