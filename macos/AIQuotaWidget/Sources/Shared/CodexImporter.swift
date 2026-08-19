import Foundation

enum CodexImporter {
    struct Result: Sendable {
        var values: [String: LiveQuotaValue]
        var fileCount: Int
    }

    private struct Candidate {
        var value: LiveQuotaValue
        var fileDate: Date
        var sequence: Int
    }

    static func scan(paths: [String]) -> Result {
        let databasePaths = paths
            .map(SnapshotStore.expandPath)
            .filter { $0.pathExtension.lowercased() == "sqlite" || $0.lastPathComponent.contains("state") }

        var rolloutPaths: Set<URL> = []
        for database in databasePaths where FileManager.default.fileExists(atPath: database.path) {
            for path in queryRolloutPaths(database: database) {
                let url = URL(fileURLWithPath: path).standardizedFileURL
                if FileManager.default.fileExists(atPath: url.path) {
                    rolloutPaths.insert(url)
                }
            }
        }

        for path in paths.map(SnapshotStore.expandPath) where path.pathExtension.lowercased() == "jsonl" {
            if FileManager.default.fileExists(atPath: path.path) { rolloutPaths.insert(path) }
        }

        var candidates: [String: Candidate] = [:]
        let sortedPaths = rolloutPaths.sorted { fileDate($0) < fileDate($1) }
        var sequence = 0
        for rollout in sortedPaths {
            let date = fileDate(rollout)
            for live in readRateLimits(from: rollout) {
                sequence += 1
                let key = windowID(minutes: live.windowMinutes)
                candidates[key] = Candidate(value: live, fileDate: date, sequence: sequence)
            }
        }

        return Result(
            values: candidates.mapValues(\.value),
            fileCount: rolloutPaths.count
        )
    }

    private static func queryRolloutPaths(database: URL) -> [String] {
        let sql = "SELECT rollout_path FROM threads WHERE tokens_used > 0 ORDER BY updated_at DESC LIMIT 300;"
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/sqlite3")
        process.arguments = ["-readonly", "-noheader", database.path, sql]
        let output = Pipe()
        process.standardOutput = output
        process.standardError = Pipe()

        do {
            try process.run()
            process.waitUntilExit()
            guard process.terminationStatus == 0 else { return [] }
            let data = output.fileHandleForReading.readDataToEndOfFile()
            guard let text = String(data: data, encoding: .utf8) else { return [] }
            return text.split(whereSeparator: \.isNewline).map(String.init)
        } catch {
            return []
        }
    }

    private static func readRateLimits(from url: URL) -> [LiveQuotaValue] {
        guard let handle = try? FileHandle(forReadingFrom: url) else { return [] }
        defer { try? handle.close() }

        let size = (try? handle.seekToEnd()) ?? 0
        let start = size > 2_000_000 ? size - 2_000_000 : 0
        try? handle.seek(toOffset: start)
        guard let data = try? handle.readToEnd(),
              let text = String(data: data, encoding: .utf8) else {
            return []
        }

        var result: [LiveQuotaValue] = []
        for line in text.split(whereSeparator: { $0.isNewline }) {
            guard let lineData = line.data(using: .utf8),
                  let object = try? JSONSerialization.jsonObject(with: lineData) else { continue }
            let resetReferenceDate: Date
            if let dictionary = object as? [String: Any],
               let date = JSONSupport.date(in: dictionary) {
                resetReferenceDate = date
            } else {
                resetReferenceDate = fileDate(url)
            }
            appendRateLimits(
                from: object,
                resetReferenceDate: resetReferenceDate,
                into: &result
            )
        }
        return result
    }

    private static func appendRateLimits(
        from value: Any,
        resetReferenceDate: Date,
        into result: inout [LiveQuotaValue]
    ) {
        if let dictionary = value as? [String: Any] {
            if let limits = dictionary["rate_limits"] as? [String: Any] {
                for key in ["primary", "secondary"] {
                    guard let limit = limits[key] as? [String: Any],
                          let minutes = JSONSupport.int64(limit["window_minutes"]),
                          let percent = JSONSupport.number(limit["used_percent"]) else {
                        continue
                    }
                    let normalizedMinutes = normalizeWindowMinutes(Int(minutes))
                    let resetAt = JSONSupport.date(limit["resets_at"])
                        ?? JSONSupport.date(limit["reset_at"])
                        ?? JSONSupport.number(limit["resets_in_seconds"]).map {
                            resetReferenceDate.addingTimeInterval($0)
                        }
                    result.append(LiveQuotaValue(
                        windowMinutes: normalizedMinutes,
                        usedPercent: percent,
                        resetAt: resetAt,
                        note: "Live rate limit"
                    ))
                }
            }

            for child in dictionary.values {
                appendRateLimits(
                    from: child,
                    resetReferenceDate: resetReferenceDate,
                    into: &result
                )
            }
        } else if let array = value as? [Any] {
            for child in array {
                appendRateLimits(
                    from: child,
                    resetReferenceDate: resetReferenceDate,
                    into: &result
                )
            }
        }
    }

    private static func fileDate(_ url: URL) -> Date {
        guard let attributes = try? FileManager.default.attributesOfItem(atPath: url.path) else {
            return .distantPast
        }
        return attributes[.modificationDate] as? Date ?? .distantPast
    }

    private static func windowID(minutes: Int) -> String {
        switch normalizeWindowMinutes(minutes) {
        case 300: return "5h"
        case 10080: return "weekly"
        default: return "\(minutes)m"
        }
    }

    private static func normalizeWindowMinutes(_ minutes: Int) -> Int {
        switch minutes {
        case 299: return 300
        case 10079: return 10080
        default: return minutes
        }
    }
}
