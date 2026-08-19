import Foundation

struct ClaudeScanResult: Sendable {
    var events: [UsageEvent]
    var liveValues: [String: LiveQuotaValue]
    var usageFileCount: Int
    var quotaFileCount: Int
}

enum ClaudeImporter {
    static func scan(
        paths: [String],
        maxWindowMinutes: Int,
        now: Date
    ) -> ClaudeScanResult {
        let usage = UsageFileScanner.scan(
            paths: paths,
            kind: .claude,
            maxWindowMinutes: maxWindowMinutes,
            now: now
        )
        let quotaFiles = quotaFiles(paths: paths)
        var liveValues: [String: LiveQuotaValue] = [:]

        for file in quotaFiles {
            guard let data = try? Data(contentsOf: file),
                  let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let cached = object["cachedUsageUtilization"] as? [String: Any],
                  let utilization = cached["utilization"] as? [String: Any] else {
                continue
            }

            addLiveValue(
                from: utilization["five_hour"],
                id: "5h",
                minutes: 300,
                into: &liveValues
            )
            addLiveValue(
                from: utilization["seven_day"],
                id: "weekly",
                minutes: 10080,
                into: &liveValues
            )
        }

        return ClaudeScanResult(
            events: usage.events,
            liveValues: liveValues,
            usageFileCount: usage.fileCount,
            quotaFileCount: quotaFiles.count
        )
    }

    private static func addLiveValue(
        from rawValue: Any?,
        id: String,
        minutes: Int,
        into values: inout [String: LiveQuotaValue]
    ) {
        guard let dictionary = rawValue as? [String: Any],
              let utilization = JSONSupport.number(dictionary["utilization"]) else {
            return
        }

        values[id] = LiveQuotaValue(
            windowMinutes: minutes,
            usedPercent: utilization,
            resetAt: JSONSupport.date(dictionary["resets_at"]),
            note: "Claude local rate limit cache"
        )
    }

    private static func quotaFiles(paths: [String]) -> [URL] {
        var result = Set<URL>()
        let fileManager = FileManager.default

        for path in paths {
            let url = SnapshotStore.expandPath(path)
            var isDirectory: ObjCBool = false
            guard fileManager.fileExists(atPath: url.path, isDirectory: &isDirectory) else { continue }

            if !isDirectory.boolValue {
                if url.pathExtension.lowercased() == "json" { result.insert(url) }
                continue
            }

            if url.lastPathComponent == ".claude" {
                let sibling = url.deletingLastPathComponent().appendingPathComponent(".claude.json")
                if fileManager.fileExists(atPath: sibling.path) { result.insert(sibling) }
            }
        }

        return result.sorted { $0.path < $1.path }
    }
}
