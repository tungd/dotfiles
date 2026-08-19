import SwiftUI

struct DashboardView: View {
    @ObservedObject var model: AppModel
    @Environment(\.openWindow) private var openWindow

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 20) {
                header

                if let error = model.lastError {
                    Label(error, systemImage: "exclamationmark.triangle.fill")
                        .font(.callout)
                        .foregroundStyle(.orange)
                        .textSelection(.enabled)
                }

                if model.snapshot.providers.isEmpty {
                    emptyState
                } else {
                    ForEach(model.snapshot.providers) { provider in
                        ProviderCard(provider: provider)
                    }
                }

                DeepSeekCard(snapshot: model.snapshot.deepSeek)

                Text("Local data only · credentials are ignored")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .frame(maxWidth: .infinity, alignment: .center)
            }
            .padding(24)
        }
        .frame(minWidth: 560, minHeight: 560)
    }

    private var header: some View {
        HStack(alignment: .top) {
            VStack(alignment: .leading, spacing: 5) {
                Text("Quota")
                    .font(.largeTitle.weight(.semibold))
                Text(QuotaFormat.lastUpdated(model.snapshot.generatedAt))
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }

            Spacer()

            HStack(spacing: 8) {
                Button {
                    openWindow(id: "settings")
                } label: {
                    Label("Settings", systemImage: "slider.horizontal.3")
                }
                .buttonStyle(.borderless)

                Button {
                    model.refresh()
                } label: {
                    Label("Refresh", systemImage: "arrow.clockwise")
                }
                .buttonStyle(.borderedProminent)
                .disabled(model.isRefreshing)
            }
        }
    }

    private var emptyState: some View {
        ContentUnavailableView {
            Label("No provider data yet", systemImage: "chart.bar.xaxis")
        } description: {
            Text("Refresh after the local provider files have been written.")
        } actions: {
            Button("Refresh") { model.refresh() }
        }
    }
}

private struct ProviderCard: View {
    let provider: ProviderSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            HStack {
                Label(provider.name, systemImage: iconName)
                    .font(.headline)
                Spacer()
                StatusLabel(status: provider.status)
            }

            ForEach(provider.windows) { window in
                WindowRow(window: window)
            }

            Text(provider.detail)
                .font(.caption)
                .foregroundStyle(.secondary)
                .textSelection(.enabled)
        }
        .padding(18)
        .background(.quaternary.opacity(0.42), in: RoundedRectangle(cornerRadius: 16))
    }

    private var iconName: String {
        switch provider.source {
        case .claude: return "bubble.left.and.bubble.right"
        case .codex: return "chevron.left.forwardslash.chevron.right"
        case .huggingface: return "cloud.fill"
        case .generic: return "shippingbox"
        }
    }
}

private struct ResetCountdown: View {
    let date: Date?

    var body: some View {
        HStack(spacing: 4) {
            Image(systemName: "arrow.counterclockwise.circle")
            if let date {
                Text("Resets")
                Text(date, style: .timer)
                    .monospacedDigit()
            } else {
                Text("Reset unavailable")
            }
        }
        .font(.caption)
        .foregroundStyle(.secondary)
    }
}

private struct WindowRow: View {
    let window: WindowSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 7) {
            HStack(alignment: .firstTextBaseline) {
                Text(window.label)
                    .font(.subheadline.weight(.medium))
                Spacer()
                if let usedPercent = window.usedPercent {
                    Text("\(QuotaFormat.percent(usedPercent)) used")
                        .font(.subheadline.monospacedDigit())
                } else {
                    Text(window.sourceNote == "No local data" ? "No data" : "Unavailable")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
            }

            ProgressView(value: window.usedPercent ?? 0, total: 100)
                .tint(progressColor)
                .opacity(window.usedPercent == nil ? 0.35 : 1)

            HStack(alignment: .firstTextBaseline, spacing: 10) {
                if let headroom = window.headroomPercent {
                    Text("\(QuotaFormat.percent(headroom)) headroom")
                        .font(.caption.weight(.medium))
                }
                if window.usedTokens != nil {
                    Text(QuotaFormat.tokenLine(window))
                        .font(.caption.monospacedDigit())
                        .foregroundStyle(.secondary)
                }
                Spacer()
                ResetCountdown(date: window.resetAt)
            }

            HStack(spacing: 8) {
                Image(systemName: paceIcon)
                Text(QuotaFormat.pace(window.paceDeltaPercentagePoints))
                if let burn = window.burnRateTokensPerHour {
                    Text("· \(QuotaFormat.compactTokens(Int64(burn.rounded()))) tokens/h")
                } else if let burn = window.burnRatePercentPerHour {
                    Text("· \(String(format: "%.1f", burn)) pp/h")
                }
            }
            .font(.caption)
            .foregroundStyle(paceColor)
        }
        .accessibilityElement(children: .combine)
        .accessibilityLabel(accessibilityLabel)
    }

    private var progressColor: Color {
        guard let used = window.usedPercent else { return .secondary }
        if used >= 85 { return .orange }
        return .accentColor
    }

    private var paceColor: Color {
        guard let pace = window.paceDeltaPercentagePoints else { return .secondary }
        return pace > 0.5 ? .orange : .secondary
    }

    private var paceIcon: String {
        guard let pace = window.paceDeltaPercentagePoints else { return "minus" }
        return pace > 0.5 ? "arrow.up.right" : pace < -0.5 ? "arrow.down.right" : "equal"
    }

    private var accessibilityLabel: String {
        let usage = window.usedPercent.map { QuotaFormat.percent($0) } ?? "unknown"
        return "\(window.label), \(usage) used, \(QuotaFormat.pace(window.paceDeltaPercentagePoints)), \(QuotaFormat.reset(window.resetAt))"
    }
}

private struct StatusLabel: View {
    let status: String

    var body: some View {
        Label(label, systemImage: icon)
            .font(.caption.weight(.medium))
            .foregroundStyle(color)
    }

    private var label: String {
        switch status {
        case "ready": return "Ready"
        case "data-only": return "Add limits"
        default: return "Needs setup"
        }
    }

    private var icon: String {
        status == "ready" ? "checkmark.circle.fill" : "questionmark.circle"
    }

    private var color: Color {
        status == "ready" ? .green : .secondary
    }
}

private struct DeepSeekCard: View {
    let snapshot: DeepSeekSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack {
                Label("DeepSeek", systemImage: "bolt.horizontal.circle")
                    .font(.headline)
                Spacer()
                Text(stateLabel)
                    .font(.caption.weight(.medium))
                    .foregroundStyle(stateColor)
            }
            Text(snapshot.windowDescription)
                .font(.subheadline)
            if let nextChangeAt = snapshot.nextChangeAt {
                Text("\(snapshot.state == .peak ? "Peak ends" : "Peak starts") \(QuotaFormat.dateTime(nextChangeAt))")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            Text(snapshot.detail)
                .font(.caption)
                .foregroundStyle(.secondary)
        }
        .padding(18)
        .background(.quaternary.opacity(0.42), in: RoundedRectangle(cornerRadius: 16))
    }

    private var stateLabel: String {
        switch snapshot.state {
        case .peak: return "Peak now"
        case .outsidePeak: return "Outside peak"
        case .notConfigured: return "Set schedule"
        }
    }

    private var stateColor: Color {
        switch snapshot.state {
        case .peak: return .orange
        case .outsidePeak: return .green
        case .notConfigured: return .secondary
        }
    }
}

struct MenuBarView: View {
    @ObservedObject var model: AppModel
    @Environment(\.openWindow) private var openWindow

    var body: some View {
        Button("Open Quota") { openWindow(id: "dashboard") }
        Button("Refresh") { model.refresh() }
        Divider()
        Text(menuSummary)
            .foregroundStyle(.secondary)
        Button("Settings…") { openWindow(id: "settings") }
        Divider()
        Button("Quit") { NSApplication.shared.terminate(nil) }
    }

    private var menuSummary: String {
        let ready = model.snapshot.providers.filter { $0.status == "ready" }.count
        return "\(ready)/\(model.snapshot.providers.count) providers with limits"
    }
}
