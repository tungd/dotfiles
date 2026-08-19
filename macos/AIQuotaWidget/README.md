# Quota

A local-only macOS WidgetKit app for watching Claude, Codex, Hugging Face
Inference, and DeepSeek's published peak windows.

The app reads local usage metadata and writes only a small rendered snapshot to
`~/Library/Application Support/AIQuotaWidget/snapshot.json`. It does not read or
display provider credentials. The default source paths are:

- Claude: `~/.claude/**/*.jsonl` token records plus `~/.claude.json` cached quota utilization
- Codex: `~/.codex/state_5.sqlite` rollout paths and their `rate_limits` records
- Hugging Face: `~/.cache/huggingface` files named `usage`, `quota`, or `limit`

Hugging Face plan limits are provider/account-specific. Add them in the in-app
Settings JSON using `limitTokens` and, when known, `resetAt`:

```json
{
  "id": "5h",
  "label": "5h",
  "minutes": 300,
  "limitTokens": 4000000,
  "resetAt": "2026-08-19T15:00:00Z"
}
```

For a generic provider, a local JSON file can use this shape:

```json
{
  "windows": {
    "5h": {
      "usedTokens": 120000,
      "limitTokens": 1000000,
      "resetAt": "2026-08-19T15:00:00Z"
    },
    "weekly": {
      "usedPercent": 32,
      "resetAt": "2026-08-24T00:00:00Z"
    }
  }
}
```

DeepSeek's published API peak windows are 01:00–04:00 and 06:00–10:00 UTC.
The default config converts those windows to 08:00–11:00 and 13:00–17:00 in
`Asia/Ho_Chi_Minh`. The `peakWindows` entries are editable if you use another
timezone or DeepSeek changes the schedule.

## Build and install locally

```sh
./scripts/install-local.sh
```

That generates the Xcode project, builds a local development app, copies it to
`~/Applications/AI Quota.app`, and launches it. Open the macOS widget gallery and
add **Quota** to the desktop or Notification Center after the first launch.

The app is not submitted to App Store Connect and has no public distribution
step.

## Emacs dashboard

The Emacs configuration includes a read-only `*Quota*` buffer backed by the
same local snapshot. Open it with `M-x quota-dashboard` or `C-l q`; `g`/`r`
refreshes it manually and it refreshes automatically every 30 seconds.
