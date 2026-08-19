import Foundation

enum AntigravityImporter {
    struct Result: Sendable {
        var values: [String: LiveQuotaValue]
        var detail: String
    }

    private struct GroupContext {
        var name: String
        var key: String?
    }

    static func scan(paths: [String]) -> Result {
        guard let executable = executableURL(paths: paths) else {
            return Result(
                values: [:],
                detail: "agy CLI not found. Expected it in the configured path or ~/.local/bin/agy."
            )
        }

        guard let data = runUsage(executable: executable) else {
            return Result(
                values: [:],
                detail: "agy CLI could not return quota data from /usage."
            )
        }

        let values = parseUsage(data: data)
        guard !values.isEmpty else {
            return Result(
                values: [:],
                detail: "agy /usage returned no recognized quota buckets."
            )
        }

        return Result(
            values: values,
            detail: "Reads Google AI Pro quota through the installed agy CLI /usage command."
        )
    }

    static func parseUsage(data: Data) -> [String: LiveQuotaValue] {
        guard let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let command = root["command"] as? [String: Any],
              let commandData = command["data"] as? [String: Any],
              let groups = commandData["groups"] as? [Any] else {
            return [:]
        }

        var values: [String: LiveQuotaValue] = [:]
        for groupValue in groups {
            guard let group = groupValue as? [String: Any],
                  let groupName = string(group["name"]) else {
                continue
            }

            let context = GroupContext(name: groupName, key: groupKey(for: groupName))
            guard let buckets = group["buckets"] as? [Any] else { continue }
            for bucketValue in buckets {
                guard let bucket = bucketValue as? [String: Any],
                      let value = parseBucket(bucket, context: context) else {
                    continue
                }

                for key in value.keys {
                    values[key] = value.value
                }
            }
        }
        return values
    }

    private static func parseBucket(
        _ bucket: [String: Any],
        context: GroupContext
    ) -> (keys: Set<String>, value: LiveQuotaValue)? {
        guard let remaining = remainingFraction(in: bucket),
              let window = windowKey(in: bucket) else {
            return nil
        }

        let usedPercent = min(max((1 - remaining) * 100, 0), 100)
        let resetAt = [
            "reset_time", "resetTime", "reset_at", "resetAt", "resets_at", "resetsAt"
        ].compactMap { JSONSupport.date(bucket[$0]) }.first
        let groupName = context.name
        let bucketID = string(bucket["id"])?.lowercased()
        var keys = Set<String>()
        if let bucketID, !bucketID.isEmpty {
            keys.insert(bucketID)
        }
        if let groupKey = context.key {
            keys.insert(groupKey + "-" + window)
        }
        guard !keys.isEmpty else { return nil }

        return (
            keys: keys,
            value: LiveQuotaValue(
                windowMinutes: window == "weekly" ? 10080 : 300,
                usedPercent: usedPercent,
                resetAt: resetAt,
                note: "Antigravity agy rate limit (" + groupName + ")"
            )
        )
    }

    private static func remainingFraction(in bucket: [String: Any]) -> Double? {
        let fractionKeys = ["remaining_fraction", "remainingFraction"]
        for key in fractionKeys {
            if let value = JSONSupport.number(bucket[key]) {
                return value > 1 ? value / 100 : value
            }
        }

        let percentKeys = ["remaining_percent", "remainingPercent"]
        for key in percentKeys {
            if let value = JSONSupport.number(bucket[key]) {
                return value / 100
            }
        }
        return nil
    }

    private static func windowKey(in bucket: [String: Any]) -> String? {
        let values = [
            string(bucket["window"]),
            string(bucket["id"]),
            string(bucket["name"])
        ].compactMap { $0?.lowercased() }

        if values.contains(where: { value in
            value.contains("weekly") || value.contains("week")
        }) {
            return "weekly"
        }
        if values.contains(where: { value in
            value.contains("5h") || value.contains("fivehour") || value.contains("five hour")
        }) {
            return "5h"
        }
        return nil
    }

    private static func groupKey(for name: String) -> String? {
        let normalized = name.lowercased()
        if normalized.contains("gemini") { return "gemini" }
        if normalized.contains("claude") || normalized.contains("gpt") { return "3p" }
        return nil
    }

    private static func string(_ value: Any?) -> String? {
        value as? String
    }

    private static func executableURL(paths: [String]) -> URL? {
        let home = FileManager.default.homeDirectoryForCurrentUser
        var candidates = paths.map(SnapshotStore.expandPath)
        candidates.append(home.appendingPathComponent(".local/bin/agy"))
        candidates.append(URL(fileURLWithPath: "/opt/homebrew/bin/agy"))
        candidates.append(URL(fileURLWithPath: "/usr/local/bin/agy"))

        for candidate in candidates {
            var isDirectory: ObjCBool = false
            if FileManager.default.fileExists(atPath: candidate.path, isDirectory: &isDirectory),
               !isDirectory.boolValue,
               FileManager.default.isExecutableFile(atPath: candidate.path) {
                return candidate
            }
        }
        return nil
    }

    private static func runUsage(executable: URL) -> Data? {
        let process = Process()
        process.executableURL = executable
        process.arguments = ["--print", "/usage", "--output-format", "json"]

        var environment = ProcessInfo.processInfo.environment
        environment["NO_COLOR"] = "1"
        process.environment = environment

        let output = Pipe()
        process.standardOutput = output
        process.standardError = Pipe()

        do {
            try process.run()
            process.waitUntilExit()
        } catch {
            return nil
        }

        guard process.terminationStatus == 0 else { return nil }
        let data = output.fileHandleForReading.readDataToEndOfFile()
        return data.isEmpty ? nil : data
    }
}
