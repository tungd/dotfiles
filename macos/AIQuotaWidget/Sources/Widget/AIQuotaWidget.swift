import SwiftUI
import WidgetKit

struct AIQuotaTimelineEntry: TimelineEntry {
    let date: Date
    let snapshot: DashboardSnapshot
}

struct AIQuotaProvider: TimelineProvider {
    func placeholder(in context: Context) -> AIQuotaTimelineEntry {
        AIQuotaTimelineEntry(date: Date(), snapshot: placeholderSnapshot)
    }

    func getSnapshot(in context: Context, completion: @escaping (AIQuotaTimelineEntry) -> Void) {
        completion(AIQuotaTimelineEntry(date: Date(), snapshot: displaySnapshot()))
    }

    func getTimeline(in context: Context, completion: @escaping (Timeline<AIQuotaTimelineEntry>) -> Void) {
        let now = Date()
        let config = SnapshotStore.loadConfig()
        let snapshot = SnapshotPresentation.aligned(SnapshotStore.loadSnapshot(), to: config)
        let refreshMinutes = max(5, config.refreshMinutes)
        let next = now.addingTimeInterval(Double(refreshMinutes) * 60)
        completion(Timeline(entries: [AIQuotaTimelineEntry(date: now, snapshot: snapshot)], policy: .after(next)))
    }

    private func displaySnapshot() -> DashboardSnapshot {
        let config = SnapshotStore.loadConfig()
        return SnapshotPresentation.aligned(SnapshotStore.loadSnapshot(), to: config)
    }

    private var placeholderSnapshot: DashboardSnapshot {
        DashboardSnapshot(
            generatedAt: Date(),
            providers: [
                ProviderSnapshot(
                    id: "codex",
                    name: "Codex",
                    source: .codex,
                    status: "ready",
                    windows: [
                        WindowSnapshot(
                            id: "5h",
                            label: "5h",
                            windowMinutes: 300,
                            usedTokens: nil,
                            limitTokens: nil,
                            usedPercent: 34,
                            headroomPercent: 66,
                            resetAt: Date().addingTimeInterval(3 * 3600),
                            burnRateTokensPerHour: nil,
                            burnRatePercentPerHour: 6,
                            paceDeltaPercentagePoints: -8,
                            sourceNote: "Live rate limit"
                        )
                    ],
                    detail: ""
                )
            ],
            deepSeek: DeepSeekSnapshot(
                state: .outsidePeak,
                timezone: TimeZone.current.identifier,
                windowDescription: "18:00–22:00 local",
                nextChangeAt: Date().addingTimeInterval(3600),
                detail: ""
            )
        )
    }
}

struct AIQuotaWidgetView: View {
    let entry: AIQuotaTimelineEntry
    @Environment(\.widgetFamily) private var family

    var body: some View {
        Group {
            switch family {
            case .systemSmall:
                SmallWidget(snapshot: entry.snapshot)
            case .systemMedium:
                MediumWidget(snapshot: entry.snapshot)
            default:
                LargeWidget(snapshot: entry.snapshot)
            }
        }
        .containerBackground(.fill.tertiary, for: .widget)
        .widgetURL(URL(string: "aiquotawidget://dashboard"))
    }
}

private struct SmallWidget: View {
    let snapshot: DashboardSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Label("Quota", systemImage: "gauge.with.dots.needle.67percent")
                .font(.caption.weight(.semibold))
            ForEach(rows.prefix(3), id: \.id) { row in
                WidgetWindowLine(row: row)
            }
            Spacer(minLength: 0)
            Text(QuotaFormat.lastUpdated(snapshot.generatedAt))
                .font(.caption2)
                .foregroundStyle(.secondary)
        }
    }

    private var rows: [WidgetRow] {
        snapshot.providers.flatMap { provider in
            provider.windows.map { WidgetRow(id: "\(provider.id)-\($0.id)", title: "\(provider.name) \($0.label)", window: $0) }
        }
    }
}

private struct MediumWidget: View {
    let snapshot: DashboardSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack {
                Label("Quota", systemImage: "gauge.with.dots.needle.67percent")
                    .font(.headline)
                Spacer()
                Text(QuotaFormat.lastUpdated(snapshot.generatedAt))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }
            LazyVGrid(columns: [GridItem(.flexible()), GridItem(.flexible())], alignment: .leading, spacing: 10) {
                ForEach(rows.prefix(6), id: \.id) { row in
                    WidgetWindowLine(row: row)
                }
            }
        }
    }

    private var rows: [WidgetRow] {
        snapshot.providers.flatMap { provider in
            provider.windows.map { WidgetRow(id: "\(provider.id)-\($0.id)", title: "\(provider.name) \($0.label)", window: $0) }
        }
    }
}

private struct LargeWidget: View {
    let snapshot: DashboardSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                Label("Quota", systemImage: "gauge.with.dots.needle.67percent")
                    .font(.headline)
                Spacer()
                Text(QuotaFormat.lastUpdated(snapshot.generatedAt))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }

            ForEach(snapshot.providers) { provider in
                VStack(alignment: .leading, spacing: 6) {
                    Text(provider.name)
                        .font(.caption.weight(.semibold))
                    ForEach(provider.windows) { window in
                        WindowLine(window: window)
                    }
                }
            }

            Divider()
            VStack(alignment: .leading, spacing: 4) {
                HStack {
                    Text("DeepSeek")
                        .font(.caption.weight(.semibold))
                    Spacer()
                    Text(deepSeekStatusLabel)
                        .font(.caption)
                }
                Text(snapshot.deepSeek.windowDescription)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .minimumScaleFactor(0.8)
            }
        }
    }

    private var deepSeekStatusLabel: String {
        switch snapshot.deepSeek.state {
        case .peak:
            return "Peak now"
        case .outsidePeak:
            return "Outside peak"
        case .notConfigured:
            return "Schedule not set"
        }
    }
}

private struct WidgetRow: Identifiable {
    let id: String
    let title: String
    let window: WindowSnapshot
}

private struct WidgetWindowLine: View {
    let row: WidgetRow

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            HStack {
                Text(row.title)
                    .font(.caption.weight(.medium))
                    .lineLimit(1)
                Spacer()
                Text(QuotaFormat.percent(row.window.usedPercent))
                    .font(.caption.monospacedDigit())
            }
            ProgressView(value: row.window.usedPercent ?? 0, total: 100)
                .tint((row.window.usedPercent ?? 0) > 85 ? .orange : .accentColor)
                .opacity(row.window.usedPercent == nil ? 0.35 : 1)
            WidgetResetCountdown(date: row.window.resetAt)
        }
        .accessibilityElement(children: .combine)
    }
}

private struct WidgetResetCountdown: View {
    let date: Date?

    var body: some View {
        HStack(spacing: 3) {
            Image(systemName: "arrow.counterclockwise.circle")
            if let date {
                Text("Reset")
                Text(date, style: .timer)
                    .monospacedDigit()
            } else {
                Text("Reset unavailable")
            }
        }
        .font(.caption2)
        .foregroundStyle(.secondary)
    }
}

private struct WindowLine: View {
    let window: WindowSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 3) {
            HStack(spacing: 8) {
                Text(window.label)
                    .font(.caption2.weight(.medium))
                    .frame(width: 44, alignment: .leading)
                ProgressView(value: window.usedPercent ?? 0, total: 100)
                    .tint((window.usedPercent ?? 0) > 85 ? .orange : .accentColor)
                    .opacity(window.usedPercent == nil ? 0.35 : 1)
                Text(QuotaFormat.percent(window.usedPercent))
                    .font(.caption2.monospacedDigit())
                    .frame(width: 36, alignment: .trailing)
            }
            WidgetResetCountdown(date: window.resetAt)
        }
    }
}

private extension AIQuotaWidget {
    static var supportedFamilies: [WidgetFamily] { [.systemSmall, .systemMedium, .systemLarge] }
}

struct AIQuotaWidget: Widget {
    let kind = "com.tung.aiquotawidget.status"

    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: AIQuotaProvider()) { entry in
            AIQuotaWidgetView(entry: entry)
        }
        .configurationDisplayName("Quota")
        .description("See quota usage, reset countdowns, and DeepSeek peak hours.")
        .supportedFamilies(Self.supportedFamilies)
    }
}

@main
struct AIQuotaWidgetBundle: WidgetBundle {
    var body: some Widget {
        AIQuotaWidget()
    }
}
