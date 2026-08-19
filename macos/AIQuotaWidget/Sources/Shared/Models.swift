import Foundation

enum SourceKind: String, Codable, CaseIterable, Sendable {
    case claude
    case codex
    case huggingface
    case antigravity
    case generic
}

struct QuotaWindowConfig: Codable, Equatable, Sendable {
    var id: String
    var label: String
    var minutes: Int
    var limitTokens: Int64?
    var resetAt: Date?

    init(
        id: String,
        label: String,
        minutes: Int,
        limitTokens: Int64? = nil,
        resetAt: Date? = nil
    ) {
        self.id = id
        self.label = label
        self.minutes = minutes
        self.limitTokens = limitTokens
        self.resetAt = resetAt
    }

    enum CodingKeys: String, CodingKey {
        case id, label, minutes, limitTokens, resetAt
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        id = try container.decode(String.self, forKey: .id)
        label = try container.decodeIfPresent(String.self, forKey: .label) ?? id
        minutes = try container.decode(Int.self, forKey: .minutes)
        limitTokens = try container.decodeIfPresent(Int64.self, forKey: .limitTokens)
        resetAt = try container.decodeIfPresent(Date.self, forKey: .resetAt)
    }
}

struct ProviderConfig: Codable, Equatable, Sendable, Identifiable {
    var id: String
    var name: String
    var source: SourceKind
    var paths: [String]
    var enabled: Bool
    var windows: [QuotaWindowConfig]

    init(
        id: String,
        name: String,
        source: SourceKind,
        paths: [String],
        enabled: Bool = true,
        windows: [QuotaWindowConfig]
    ) {
        self.id = id
        self.name = name
        self.source = source
        self.paths = paths
        self.enabled = enabled
        self.windows = windows
    }

    enum CodingKeys: String, CodingKey {
        case id, name, source, paths, enabled, windows
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        id = try container.decode(String.self, forKey: .id)
        name = try container.decodeIfPresent(String.self, forKey: .name) ?? id
        source = try container.decodeIfPresent(SourceKind.self, forKey: .source) ?? .generic
        paths = try container.decodeIfPresent([String].self, forKey: .paths) ?? []
        enabled = try container.decodeIfPresent(Bool.self, forKey: .enabled) ?? true
        windows = try container.decodeIfPresent([QuotaWindowConfig].self, forKey: .windows) ?? []
    }
}

struct DeepSeekPeakWindow: Codable, Equatable, Sendable {
    var start: String
    var end: String

    init(start: String, end: String) {
        self.start = start
        self.end = end
    }
}

struct DeepSeekSchedule: Codable, Equatable, Sendable {
    var timezone: String
    var peakWindows: [DeepSeekPeakWindow]
    var days: [Int]?

    init(
        timezone: String = TimeZone.current.identifier,
        peakWindows: [DeepSeekPeakWindow] = [],
        days: [Int]? = nil
    ) {
        self.timezone = timezone
        self.peakWindows = peakWindows
        self.days = days
    }

    init(
        timezone: String = TimeZone.current.identifier,
        peakStart: String?,
        peakEnd: String?,
        days: [Int]? = nil
    ) {
        self.timezone = timezone
        self.peakWindows = [
            peakStart.flatMap { start in
                peakEnd.map { DeepSeekPeakWindow(start: start, end: $0) }
            }
        ].compactMap { $0 }
        self.days = days
    }

    enum CodingKeys: String, CodingKey {
        case timezone, peakWindows, peakStart, peakEnd, days
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        timezone = try container.decodeIfPresent(String.self, forKey: .timezone) ?? TimeZone.current.identifier
        days = try container.decodeIfPresent([Int].self, forKey: .days)

        if let windows = try container.decodeIfPresent([DeepSeekPeakWindow].self, forKey: .peakWindows) {
            peakWindows = windows
        } else if let start = try container.decodeIfPresent(String.self, forKey: .peakStart),
                  let end = try container.decodeIfPresent(String.self, forKey: .peakEnd) {
            peakWindows = [DeepSeekPeakWindow(start: start, end: end)]
        } else {
            peakWindows = []
        }
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(timezone, forKey: .timezone)
        try container.encode(peakWindows, forKey: .peakWindows)
        try container.encodeIfPresent(days, forKey: .days)
    }
}

struct WidgetConfig: Codable, Equatable, Sendable {
    var refreshMinutes: Int
    var providers: [ProviderConfig]
    var deepSeek: DeepSeekSchedule

    init(
        refreshMinutes: Int = 10,
        providers: [ProviderConfig] = [],
        deepSeek: DeepSeekSchedule = DeepSeekSchedule()
    ) {
        self.refreshMinutes = refreshMinutes
        self.providers = providers
        self.deepSeek = deepSeek
    }

    enum CodingKeys: String, CodingKey {
        case refreshMinutes, providers, deepSeek
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        refreshMinutes = max(1, try container.decodeIfPresent(Int.self, forKey: .refreshMinutes) ?? 10)
        providers = try container.decodeIfPresent([ProviderConfig].self, forKey: .providers) ?? []
        deepSeek = try container.decodeIfPresent(DeepSeekSchedule.self, forKey: .deepSeek) ?? DeepSeekSchedule()
    }
}

struct UsageEvent: Sendable {
    var date: Date
    var totalTokens: Int64
}

struct LiveQuotaValue: Sendable {
    var windowMinutes: Int
    var usedPercent: Double
    var resetAt: Date?
    var note: String
}

struct WindowSnapshot: Codable, Equatable, Sendable, Identifiable {
    var id: String
    var label: String
    var windowMinutes: Int
    var usedTokens: Int64?
    var limitTokens: Int64?
    var usedPercent: Double?
    var headroomPercent: Double?
    var resetAt: Date?
    var burnRateTokensPerHour: Double?
    var burnRatePercentPerHour: Double?
    var paceDeltaPercentagePoints: Double?
    var sourceNote: String

    var hasLimit: Bool {
        usedPercent != nil && (limitTokens != nil || sourceNote.contains("rate limit"))
    }

    var hasData: Bool {
        usedTokens != nil || usedPercent != nil
    }
}

struct ProviderSnapshot: Codable, Equatable, Sendable, Identifiable {
    var id: String
    var name: String
    var source: SourceKind
    var status: String
    var windows: [WindowSnapshot]
    var detail: String
}

enum DeepSeekState: String, Codable, Sendable {
    case peak
    case outsidePeak
    case notConfigured
}

struct DeepSeekSnapshot: Codable, Equatable, Sendable {
    var state: DeepSeekState
    var timezone: String
    var windowDescription: String
    var nextChangeAt: Date?
    var detail: String
}

struct DashboardSnapshot: Codable, Equatable, Sendable {
    var generatedAt: Date
    var providers: [ProviderSnapshot]
    var deepSeek: DeepSeekSnapshot

    static var empty: DashboardSnapshot {
        DashboardSnapshot(
            generatedAt: Date.distantPast,
            providers: [],
            deepSeek: DeepSeekSnapshot(
                state: .notConfigured,
                timezone: TimeZone.current.identifier,
                windowDescription: "No schedule",
                nextChangeAt: nil,
                detail: "Configure a local peak window in Settings."
            )
        )
    }
}
