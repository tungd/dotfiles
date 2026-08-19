import SwiftUI
import WidgetKit
import AppIntents

enum QuotaWidgetSelection: String, AppEnum {
    case overview
    case claude
    case codex
    case huggingFace
    case gemini
    case thirdParty
    case deepSeek

    static var typeDisplayRepresentation: TypeDisplayRepresentation {
        "Quota group"
    }

    static var caseDisplayRepresentations: [QuotaWidgetSelection: DisplayRepresentation] {
        [
            .overview: "Overview",
            .claude: "Claude",
            .codex: "Codex",
            .huggingFace: "HF Pro Inference",
            .gemini: "Gemini (Antigravity)",
            .thirdParty: "Claude/GPT (Antigravity)",
            .deepSeek: "DeepSeek"
        ]
    }
}

struct QuotaWidgetIntent: WidgetConfigurationIntent {
    static var title: LocalizedStringResource { "Quota group" }
    static var description: IntentDescription { "Choose which quota group this widget instance shows." }

    @Parameter(title: "Show", default: .claude)
    var selection: QuotaWidgetSelection
}

struct AIQuotaTimelineEntry: TimelineEntry {
    let date: Date
    let snapshot: DashboardSnapshot
    let selection: QuotaWidgetSelection

    init(
        date: Date,
        snapshot: DashboardSnapshot,
        selection: QuotaWidgetSelection = .overview
    ) {
        self.date = date
        self.snapshot = snapshot
        self.selection = selection
    }
}

struct AIQuotaProvider: AppIntentTimelineProvider {
    typealias Intent = QuotaWidgetIntent

    func placeholder(in context: Context) -> AIQuotaTimelineEntry {
        AIQuotaTimelineEntry(date: Date(), snapshot: placeholderSnapshot, selection: .claude)
    }

    func snapshot(for configuration: QuotaWidgetIntent, in context: Context) async -> AIQuotaTimelineEntry {
        AIQuotaTimelineEntry(
            date: Date(),
            snapshot: displaySnapshot(selection: configuration.selection),
            selection: configuration.selection
        )
    }

    func timeline(for configuration: QuotaWidgetIntent, in context: Context) async -> Timeline<AIQuotaTimelineEntry> {
        let now = Date()
        let config = SnapshotStore.loadConfig()
        let snapshot = configuration.selection.filteredSnapshot(
            SnapshotPresentation.aligned(SnapshotStore.loadSnapshot(), to: config)
        )
        let refreshMinutes = max(5, config.refreshMinutes)
        let next = now.addingTimeInterval(Double(refreshMinutes) * 60)
        return Timeline(
            entries: [AIQuotaTimelineEntry(date: now, snapshot: snapshot, selection: configuration.selection)],
            policy: .after(next)
        )
    }

    private func displaySnapshot(selection: QuotaWidgetSelection) -> DashboardSnapshot {
        let config = SnapshotStore.loadConfig()
        return selection.filteredSnapshot(
            SnapshotPresentation.aligned(SnapshotStore.loadSnapshot(), to: config)
        )
    }

    private var placeholderSnapshot: DashboardSnapshot {
        DashboardSnapshot(
            generatedAt: Date(),
            providers: [
                ProviderSnapshot(
                    id: "claude",
                    name: "Claude",
                    source: .claude,
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

private extension QuotaWidgetSelection {
    func filteredSnapshot(_ snapshot: DashboardSnapshot) -> DashboardSnapshot {
        let providers: [ProviderSnapshot]
        switch self {
        case .overview:
            return snapshot
        case .claude:
            providers = [quotaProvider(from: snapshot, id: "claude")].compactMap { $0 }
        case .codex:
            providers = [quotaProvider(from: snapshot, id: "codex")].compactMap { $0 }
        case .huggingFace:
            providers = [quotaProvider(from: snapshot, id: "huggingface")].compactMap { $0 }
        case .gemini:
            providers = [quotaProvider(
                from: snapshot,
                id: "antigravity",
                name: "Gemini",
                windowIDs: ["gemini-5h", "gemini-weekly"]
            )].compactMap { $0 }
        case .thirdParty:
            providers = [quotaProvider(
                from: snapshot,
                id: "antigravity",
                name: "Claude/GPT",
                windowIDs: ["3p-5h", "3p-weekly"]
            )].compactMap { $0 }
        case .deepSeek:
            providers = []
        }

        return DashboardSnapshot(
            generatedAt: snapshot.generatedAt,
            providers: providers,
            deepSeek: snapshot.deepSeek
        )
    }
}

private func quotaProvider(
    from snapshot: DashboardSnapshot,
    id: String,
    name: String? = nil,
    windowIDs: Set<String>? = nil
) -> ProviderSnapshot? {
    guard var provider = snapshot.providers.first(where: { $0.id == id }) else { return nil }
    if let windowIDs {
        provider.windows = provider.windows.filter { windowIDs.contains($0.id) }
    }
    if let name {
        provider.name = name
        provider.windows = provider.windows.map { window in
            var compact = window
            if window.id.contains("5h") { compact.label = "5h" }
            if window.id.contains("weekly") { compact.label = "Weekly" }
            return compact
        }
    }
    return provider
}

struct AIQuotaWidgetView: View {
    let entry: AIQuotaTimelineEntry
    @Environment(\.widgetFamily) private var family

    var body: some View {
        Group {
            if entry.selection == .deepSeek {
                DeepSeekWidget(snapshot: entry.snapshot)
            } else {
                switch family {
                case .systemSmall:
                    SmallWidget(snapshot: entry.snapshot)
                case .systemMedium:
                    MediumWidget(snapshot: entry.snapshot)
                default:
                    LargeWidget(
                        snapshot: entry.snapshot,
                        showsDeepSeek: entry.selection == .overview
                    )
                }
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
    let showsDeepSeek: Bool

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

            if showsDeepSeek {
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

private struct DeepSeekWidget: View {
    let snapshot: DashboardSnapshot

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Label("DeepSeek", systemImage: "clock")
                .font(.headline)
            Text(statusLabel)
                .font(.title3.weight(.semibold))
            Text(snapshot.deepSeek.windowDescription)
                .font(.caption)
                .foregroundStyle(.secondary)
                .lineLimit(2)
            if let nextChangeAt = snapshot.deepSeek.nextChangeAt {
                HStack(spacing: 4) {
                    Image(systemName: "arrow.counterclockwise.circle")
                    Text("Changes")
                    Text(nextChangeAt, style: .timer)
                        .monospacedDigit()
                }
                .font(.caption2)
                .foregroundStyle(.secondary)
            }
        }
    }

    private var statusLabel: String {
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
    let kind = "com.tung.aiquotawidget.status.v2"

    var body: some WidgetConfiguration {
        AppIntentConfiguration(kind: kind, intent: QuotaWidgetIntent.self, provider: AIQuotaProvider()) { entry in
            AIQuotaWidgetView(entry: entry)
        }
        .configurationDisplayName("Quota")
        .description("Add multiple Quota widgets and choose a quota group for each.")
        .supportedFamilies(Self.supportedFamilies)
    }
}

@main
struct AIQuotaWidgetBundle: WidgetBundle {
    var body: some Widget {
        AIQuotaWidget()
    }
}
