# Codex OAuth Is The Core Auth Path

The first useful Workflow Agent CLI must support Codex OAuth rather than treating OAuth as a later enhancement after API-key auth. API keys may still be useful for fallback testing, but the primary v1 authentication path should match the operator's real subscription-backed workflow and avoid building the agent around a credential mode that is not representative.

Unlike jcode, v1 does not need labeled multi-account management; it keeps one active OpenAI/Codex credential. The existing Codex CLI auth file may be read as a trusted local source without an extra consent prompt in this personal Command Workspace, but it remains read-only; the Workflow Agent CLI writes only its own auth file when running its own login flow.

Credential lookup order is: the Workflow Agent CLI's own `~/.td-agent/openai-auth.json`, then existing Codex CLI credentials at `~/.codex/auth.json`, then `OPENAI_API_KEY` as a fallback/test path.
