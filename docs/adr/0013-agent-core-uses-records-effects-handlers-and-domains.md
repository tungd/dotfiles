# Agent Core Uses Records Effects Handlers And Domains

The OCaml Workflow Agent CLI core will model protocol and domain data with records, isolate side effects behind OCaml 5 effects and handlers, and use domains for bounded parallel work. This keeps the provider loop, tools, transcript store, and future remote execution handoff testable as pure control flow while letting local execution, test execution, and later remote execution install different handlers for the same capabilities.
