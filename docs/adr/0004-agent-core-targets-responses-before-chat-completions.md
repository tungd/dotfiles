# Agent Core Targets Responses Before Chat Completions

The Workflow Agent CLI will first implement a Responses-style provider path and shape the Agent Tool Loop around normalized Responses events. Chat-completions adapters, including Alibaba Coding Plan, are deferred until the Responses path works, so the first OCaml runtime is not distorted by compatibility code for older or provider-specific transports.
