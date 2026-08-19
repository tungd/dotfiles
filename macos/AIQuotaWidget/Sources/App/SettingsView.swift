import SwiftUI

struct SettingsView: View {
    @ObservedObject var model: AppModel
    @Environment(\.openWindow) private var openWindow

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            HStack(alignment: .firstTextBaseline) {
                VStack(alignment: .leading, spacing: 4) {
                    Text("Local sources and limits")
                        .font(.title2.weight(.semibold))
                    Text("Edit the JSON only when a provider does not expose its limits in local metadata.")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
                Spacer()
                Button("Reset example") { model.resetConfigText() }
            }

            Text("Config: \(SnapshotStore.configURL.path)")
                .font(.caption.monospaced())
                .foregroundStyle(.secondary)
                .textSelection(.enabled)

            TextEditor(text: $model.configText)
                .font(.system(.body, design: .monospaced))
                .scrollContentBackground(.hidden)
                .padding(8)
                .background(.quaternary.opacity(0.4), in: RoundedRectangle(cornerRadius: 10))

            HStack {
                Button("Open config folder") { model.openConfigFolder() }
                    .buttonStyle(.borderless)
                Spacer()
                if let error = model.lastError {
                    Text(error)
                        .font(.caption)
                        .foregroundStyle(.orange)
                        .lineLimit(2)
                }
                Button("Save and refresh") { model.saveConfig() }
                    .buttonStyle(.borderedProminent)
            }

            Divider()

            VStack(alignment: .leading, spacing: 5) {
                Text("What the default importers read")
                    .font(.headline)
                Text("Claude reads its local quota cache and JSONL usage records. Codex reads local rate-limit records from its SQLite index and rollout files. Hugging Face only scans files named usage, quota, limit, or telemetry; credential files are ignored.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Text("A configured window shows headroom, reset time, burn rate, and whether usage is ahead of or behind the elapsed window percentage.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
        .padding(22)
        .frame(minWidth: 660, minHeight: 600)
    }
}
