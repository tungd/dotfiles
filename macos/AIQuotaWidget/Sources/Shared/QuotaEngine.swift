import Foundation

struct QuotaEngine: Sendable {
    let config: WidgetConfig

    func collect(now: Date = Date()) -> DashboardSnapshot {
        let providers = config.providers
            .filter(\.enabled)
            .map { collect(provider: $0, now: now) }

        return DashboardSnapshot(
            generatedAt: now,
            providers: providers,
            deepSeek: DeepSeekCalculator.snapshot(schedule: config.deepSeek, now: now)
        )
    }

    private func collect(provider: ProviderConfig, now: Date) -> ProviderSnapshot {
        let maxWindowMinutes = max(provider.windows.map(\.minutes).max() ?? 1, 1)
        let events: [UsageEvent]
        let liveValues: [String: LiveQuotaValue]
        let genericValues: [String: GenericWindowValue]
        var detail: String

        switch provider.source {
        case .claude:
            let result = ClaudeImporter.scan(
                paths: provider.paths,
                maxWindowMinutes: maxWindowMinutes,
                now: now
            )
            events = result.events
            liveValues = result.liveValues
            genericValues = [:]
            detail = result.quotaFileCount > 0
                ? "Reads Claude's local quota cache and \(result.events.count) token records."
                : result.usageFileCount == 0
                    ? "No Claude usage or quota files found."
                    : "Found \(result.events.count) local token records."

        case .codex:
            let result = CodexImporter.scan(paths: provider.paths)
            events = []
            liveValues = result.values
            genericValues = [:]
            detail = result.fileCount == 0
                ? "No Codex rate-limit records found."
                : result.values["5h"] == nil
                    ? "Reads local Codex rate-limit records; the current record has no 5h window."
                    : "Reads local Codex rate-limit records."

        case .huggingface:
            let result = HFImporter.scan(paths: provider.paths)
            events = []
            liveValues = result.values
            genericValues = [:]
            detail = result.detail

        case .antigravity:
            let result = AntigravityImporter.scan(paths: provider.paths)
            events = []
            liveValues = result.values
            genericValues = [:]
            detail = result.detail

        case .generic:
            let result = GenericUsageImporter.scan(paths: provider.paths)
            events = []
            liveValues = [:]
            genericValues = result.values
            detail = result.fileCount == 0
                ? "No usage/quota JSON file found."
                : "Reads local usage or quota JSON."
        }

        let windows = provider.windows.map { window in
            if let live = liveValues[window.id] ?? liveValues[windowKey(minutes: window.minutes)] {
                return makeLiveSnapshot(config: window, value: live, now: now)
            }

            if let generic = genericValues[window.id] ?? genericValues[windowKey(minutes: window.minutes)] {
                return makeGenericSnapshot(config: window, value: generic, now: now)
            }

            let missingSourceNote = provider.source == .codex && window.id == "5h" && liveValues["5h"] == nil
                ? "No current 5h rate-limit record"
                : nil
            return makeTokenSnapshot(
                config: window,
                events: events,
                now: now,
                missingSourceNote: missingSourceNote
            )
        }

        let hasData = windows.contains(where: \.hasData)
        let hasConfiguredLimit = windows.contains(where: { $0.usedPercent != nil && $0.hasLimit })
        let status: String
        if hasConfiguredLimit {
            status = "ready"
        } else if hasData {
            status = "data-only"
            detail += " Add plan limits in Settings to calculate headroom."
        } else {
            status = "needs-setup"
        }

        return ProviderSnapshot(
            id: provider.id,
            name: provider.name,
            source: provider.source,
            status: status,
            windows: windows,
            detail: detail
        )
    }

    private func makeLiveSnapshot(
        config: QuotaWindowConfig,
        value: LiveQuotaValue,
        now: Date
    ) -> WindowSnapshot {
        let usedPercent = min(max(value.usedPercent, 0), 100)
        let resetAt = nextReset(after: now, configured: value.resetAt, durationMinutes: config.minutes)
        let start = resetAt.map { $0.addingTimeInterval(-Double(config.minutes) * 60) }
            ?? now.addingTimeInterval(-Double(config.minutes) * 60)
        let elapsed = elapsedFraction(from: start, to: resetAt, now: now)

        return WindowSnapshot(
            id: config.id,
            label: config.label,
            windowMinutes: config.minutes,
            usedTokens: nil,
            limitTokens: nil,
            usedPercent: usedPercent,
            headroomPercent: max(0, 100 - usedPercent),
            resetAt: resetAt,
            burnRateTokensPerHour: nil,
            burnRatePercentPerHour: elapsed.flatMap { fraction in
                fraction > 0 ? usedPercent / (fraction * Double(config.minutes) / 60) : nil
            },
            paceDeltaPercentagePoints: elapsed.map { usedPercent - $0 * 100 },
            sourceNote: value.note
        )
    }

    private func makeGenericSnapshot(
        config: QuotaWindowConfig,
        value: GenericWindowValue,
        now: Date
    ) -> WindowSnapshot {
        if let usedTokens = value.usedTokens {
            return makeTokenSnapshot(
                config: QuotaWindowConfig(
                    id: config.id,
                    label: config.label,
                    minutes: config.minutes,
                    limitTokens: value.limitTokens ?? config.limitTokens,
                    resetAt: value.resetAt ?? config.resetAt
                ),
                events: [UsageEvent(date: now, totalTokens: usedTokens)],
                now: now,
                directUsedTokens: usedTokens,
                directUsedPercent: value.usedPercent,
                sourceNote: "Local usage file"
            )
        }

        let usedPercent = value.usedPercent
        let resetAt = nextReset(after: now, configured: value.resetAt ?? config.resetAt, durationMinutes: config.minutes)
        let start = resetAt.map { $0.addingTimeInterval(-Double(config.minutes) * 60) }
            ?? now.addingTimeInterval(-Double(config.minutes) * 60)
        let elapsed = elapsedFraction(from: start, to: resetAt, now: now)

        return WindowSnapshot(
            id: config.id,
            label: config.label,
            windowMinutes: config.minutes,
            usedTokens: nil,
            limitTokens: value.limitTokens ?? config.limitTokens,
            usedPercent: usedPercent,
            headroomPercent: usedPercent.map { max(0, 100 - $0) },
            resetAt: resetAt,
            burnRateTokensPerHour: nil,
            burnRatePercentPerHour: usedPercent.flatMap { percent in
                elapsed.flatMap { fraction in
                    fraction > 0 ? percent / (fraction * Double(config.minutes) / 60) : nil
                }
            },
            paceDeltaPercentagePoints: usedPercent.flatMap { value in
                elapsed.map { value - $0 * 100 }
            },
            sourceNote: "Local usage file"
        )
    }

    private func makeTokenSnapshot(
        config: QuotaWindowConfig,
        events: [UsageEvent],
        now: Date,
        directUsedTokens: Int64? = nil,
        directUsedPercent: Double? = nil,
        sourceNote: String = "Local token records",
        missingSourceNote: String? = nil
    ) -> WindowSnapshot {
        let resetAt = nextReset(after: now, configured: config.resetAt, durationMinutes: config.minutes)
        let start = resetAt.map { $0.addingTimeInterval(-Double(config.minutes) * 60) }
            ?? now.addingTimeInterval(-Double(config.minutes) * 60)
        guard directUsedTokens != nil || directUsedPercent != nil || !events.isEmpty else {
            return WindowSnapshot(
                id: config.id,
                label: config.label,
                windowMinutes: config.minutes,
                usedTokens: nil,
                limitTokens: config.limitTokens,
                usedPercent: nil,
                headroomPercent: nil,
                resetAt: resetAt,
                burnRateTokensPerHour: nil,
                burnRatePercentPerHour: nil,
                paceDeltaPercentagePoints: nil,
                sourceNote: missingSourceNote ?? "No local data"
            )
        }
        let usedTokens = directUsedTokens ?? events
            .filter { $0.date >= start && $0.date <= now }
            .reduce(Int64.zero) { $0 + $1.totalTokens }
        let limitTokens = config.limitTokens
        let usedPercent = directUsedPercent ?? limitTokens.map {
            guard $0 > 0 else { return 0 }
            return min(max(Double(usedTokens) / Double($0) * 100, 0), 100)
        }
        let elapsed = elapsedFraction(from: start, to: resetAt, now: now)
        let elapsedHours = elapsed.map { $0 * Double(config.minutes) / 60 }
            ?? (Double(config.minutes) / 60)
        let durationHours = max(elapsedHours, 1.0 / 60.0)

        return WindowSnapshot(
            id: config.id,
            label: config.label,
            windowMinutes: config.minutes,
            usedTokens: usedTokens,
            limitTokens: limitTokens,
            usedPercent: usedPercent,
            headroomPercent: usedPercent.map { max(0, 100 - $0) },
            resetAt: resetAt,
            burnRateTokensPerHour: Double(usedTokens) / durationHours,
            burnRatePercentPerHour: usedPercent.map { $0 / durationHours },
            paceDeltaPercentagePoints: usedPercent.flatMap { value in
                elapsed.map { value - $0 * 100 }
            },
            sourceNote: sourceNote
        )
    }

    private func windowKey(minutes: Int) -> String {
        switch minutes {
        case 300: return "5h"
        case 10080: return "weekly"
        default: return "\(minutes)m"
        }
    }

    private func nextReset(after now: Date, configured: Date?, durationMinutes: Int) -> Date? {
        guard var reset = configured else { return nil }
        let duration = Double(max(durationMinutes, 1)) * 60
        while reset <= now {
            reset = reset.addingTimeInterval(duration)
        }
        return reset
    }

    private func elapsedFraction(from start: Date, to end: Date?, now: Date) -> Double? {
        guard let end, end > start else { return nil }
        return min(max(now.timeIntervalSince(start) / end.timeIntervalSince(start), 0), 1)
    }
}

struct GenericWindowValue: Sendable {
    var usedTokens: Int64?
    var limitTokens: Int64?
    var usedPercent: Double?
    var resetAt: Date?
}

struct UsageScanResult: Sendable {
    var events: [UsageEvent]
    var fileCount: Int
}

enum UsageFileScanner {
    private static let ignoredComponents: Set<String> = [
        "backups", "file-history", "paste-cache", "plugins", "security"
    ]
    private static let ignoredNames: Set<String> = [
        "token", "tokens", "auth", "credentials", "secrets"
    ]

    static func scan(
        paths: [String],
        kind: SourceKind,
        maxWindowMinutes: Int,
        now: Date
    ) -> UsageScanResult {
        let files = candidateFiles(paths: paths, kind: kind)
        let cutoff = now.addingTimeInterval(-Double(maxWindowMinutes) * 60)
        var events: [UsageEvent] = []

        for file in files {
            guard let attributes = try? FileManager.default.attributesOfItem(atPath: file.path),
                  let modified = attributes[.modificationDate] as? Date,
                  modified >= cutoff else {
                continue
            }

            guard let data = try? Data(contentsOf: file),
                  let text = String(data: data, encoding: .utf8) else {
                continue
            }

            for line in text.split(whereSeparator: \.isNewline) {
                guard let lineData = line.data(using: .utf8),
                      let object = try? JSONSerialization.jsonObject(with: lineData) else {
                    continue
                }
                appendUsageEvents(
                    from: object,
                    fallbackDate: modified,
                    into: &events
                )
            }
        }

        return UsageScanResult(events: events, fileCount: files.count)
    }

    private static func candidateFiles(paths: [String], kind: SourceKind) -> [URL] {
        var result = Set<URL>()
        let fileManager = FileManager.default

        for path in paths {
            let url = SnapshotStore.expandPath(path)
            var isDirectory: ObjCBool = false
            guard fileManager.fileExists(atPath: url.path, isDirectory: &isDirectory) else { continue }

            if !isDirectory.boolValue {
                if shouldRead(url: url, kind: kind) { result.insert(url) }
                continue
            }

            guard let enumerator = fileManager.enumerator(
                at: url,
                includingPropertiesForKeys: [.isRegularFileKey, .contentModificationDateKey],
                options: [.skipsHiddenFiles]
            ) else { continue }

            for case let child as URL in enumerator {
                if child.pathComponents.contains(where: ignoredComponents.contains) { continue }
                if shouldRead(url: child, kind: kind) { result.insert(child) }
            }
        }

        return result.sorted { $0.path < $1.path }
    }

    private static func shouldRead(url: URL, kind: SourceKind) -> Bool {
        let name = url.deletingPathExtension().lastPathComponent.lowercased()
        if ignoredNames.contains(name) || name.contains("credential") || name.contains("secret") {
            return false
        }

        switch kind {
        case .claude:
            return url.pathExtension.lowercased() == "jsonl"
        case .codex:
            return false
        case .huggingface:
            return false
        case .antigravity:
            return false
        case .generic:
            let extensionName = url.pathExtension.lowercased()
            let likelyDataName = name.contains("usage") || name.contains("quota") || name.contains("limit") || name.contains("telemetry")
            return ["json", "jsonl", "ndjson"].contains(extensionName) && likelyDataName
        }
    }

    private static func appendUsageEvents(
        from value: Any,
        fallbackDate: Date,
        into events: inout [UsageEvent]
    ) {
        if let dictionary = value as? [String: Any] {
            let date = JSONSupport.date(in: dictionary) ?? fallbackDate
            if let usage = dictionary["usage"] as? [String: Any] {
                let total = JSONSupport.usageTokenTotal(usage)
                if total > 0 {
                    events.append(UsageEvent(date: date, totalTokens: total))
                }
            }

            for child in dictionary.values {
                appendUsageEvents(from: child, fallbackDate: date, into: &events)
            }
        } else if let array = value as? [Any] {
            for child in array {
                appendUsageEvents(from: child, fallbackDate: fallbackDate, into: &events)
            }
        }
    }
}

enum JSONSupport {
    static func number(_ value: Any?) -> Double? {
        if let value = value as? NSNumber { return value.doubleValue }
        if let value = value as? String { return Double(value) }
        return nil
    }

    static func int64(_ value: Any?) -> Int64? {
        guard let number = number(value) else { return nil }
        return Int64(number.rounded())
    }

    static func date(_ value: Any?) -> Date? {
        if let number = number(value) {
            return Date(timeIntervalSince1970: number > 10_000_000_000 ? number / 1000 : number)
        }
        guard let string = value as? String else { return nil }
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        if let date = formatter.date(from: string) { return date }
        formatter.formatOptions = [.withInternetDateTime]
        return formatter.date(from: string)
    }

    static func date(in dictionary: [String: Any]) -> Date? {
        for key in ["timestamp", "created_at", "createdAt", "ts", "time", "date", "updated_at"] {
            if let date = date(dictionary[key]) { return date }
        }
        return nil
    }

    static func usageTokenTotal(_ usage: [String: Any]) -> Int64 {
        let keys = [
            "input_tokens",
            "output_tokens",
            "cache_creation_input_tokens",
            "cache_read_input_tokens",
            "thinking_tokens",
            "reasoning_output_tokens"
        ]
        let sum = keys.compactMap { int64(usage[$0]) }.reduce(Int64.zero, +)
        if sum > 0 { return sum }
        return int64(usage["total_tokens"]) ?? int64(usage["totalTokens"]) ?? 0
    }
}
