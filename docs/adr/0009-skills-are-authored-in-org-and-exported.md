# Skills Are Authored In Org And Exported

The Workflow Agent CLI will not follow jcode's first-run skill import model as the source of truth for skills. Agent Skills are authored in an operator-owned org file and materialized by an org export backend, so the CLI should consume exported skill artifacts rather than own skill authoring, importing, syncing, or distribution.
