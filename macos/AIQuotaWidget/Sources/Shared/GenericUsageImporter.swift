import Foundation

enum GenericUsageImporter {
    struct Result: Sendable {
        var values: [String: GenericWindowValue]
        var fileCount: Int
    }

    static func scan(paths: [String]) -> Result {
        let files = candidateFiles(paths: paths)
        var values: [String: GenericWindowValue] = [:]

        for file in files {
            guard let data = try? Data(contentsOf: file),
                  let object = try? JSONSerialization.jsonObject(with: data) else {
                continue
            }
            collectWindows(from: object, keyHint: nil, into: &values)
        }

        return Result(values: values, fileCount: files.count)
    }

    private static func candidateFiles(paths: [String]) -> [URL] {
        var result = Set<URL>()
        let fileManager = FileManager.default

        for path in paths {
            let url = SnapshotStore.expandPath(path)
            var isDirectory: ObjCBool = false
            guard fileManager.fileExists(atPath: url.path, isDirectory: &isDirectory) else { continue }
            if !isDirectory.boolValue {
                result.insert(url)
                continue
            }

            guard let enumerator = fileManager.enumerator(
                at: url,
                includingPropertiesForKeys: [.isRegularFileKey],
                options: [.skipsHiddenFiles]
            ) else { continue }

            for case let child as URL in enumerator {
                let name = child.deletingPathExtension().lastPathComponent.lowercased()
                let extensionName = child.pathExtension.lowercased()
                let likelyDataName = name.contains("usage") || name.contains("quota") || name.contains("limit") || name.contains("telemetry")
                let ignored = name.contains("token") || name.contains("credential") || name.contains("secret") || name == "auth"
                if !ignored && likelyDataName && ["json", "jsonl", "ndjson"].contains(extensionName) {
                    result.insert(child)
                }
            }
        }

        return result.sorted { $0.path < $1.path }
    }

    private static func collectWindows(
        from value: Any,
        keyHint: String?,
        into values: inout [String: GenericWindowValue]
    ) {
        if let dictionary = value as? [String: Any] {
            let knownWindowKey = keyHint.flatMap(normalizeWindowID)
            if knownWindowKey != nil || hasUsageFields(dictionary) {
                let id = knownWindowKey ?? JSONSupport.string(dictionary["id"])?.lowercased()
                    ?? JSONSupport.string(dictionary["window"])?.lowercased()
                    ?? JSONSupport.string(dictionary["window_id"])?.lowercased()
                if let id {
                    values[id] = GenericWindowValue(
                        usedTokens: firstInt(dictionary, keys: ["usedTokens", "used_tokens", "tokensUsed", "tokens_used"]),
                        limitTokens: firstInt(dictionary, keys: ["limitTokens", "limit_tokens", "tokenLimit", "token_limit"]),
                        usedPercent: firstDouble(dictionary, keys: ["usedPercent", "used_percent", "usedPercentage", "used_percentage", "percent"]),
                        resetAt: firstDate(dictionary, keys: ["resetAt", "reset_at", "resetsAt", "resets_at", "resetDate"])
                    )
                }
            }

            for (key, child) in dictionary {
                collectWindows(from: child, keyHint: key, into: &values)
            }
        } else if let array = value as? [Any] {
            for child in array {
                collectWindows(from: child, keyHint: keyHint, into: &values)
            }
        }
    }

    private static func normalizeWindowID(_ key: String) -> String? {
        let normalized = key.lowercased().replacingOccurrences(of: "_", with: "")
        if normalized == "5h" || normalized == "fivehours" { return "5h" }
        if normalized == "weekly" || normalized == "week" || normalized == "7d" { return "weekly" }
        return nil
    }

    private static func hasUsageFields(_ dictionary: [String: Any]) -> Bool {
        let keys = Set(dictionary.keys.map { $0.lowercased() })
        return !keys.isDisjoint(with: [
            "usedtokens", "used_tokens", "tokensused", "tokens_used", "usedpercent", "used_percent",
            "usedpercentage", "used_percentage", "percent", "limittokens", "limit_tokens"
        ])
    }

    private static func firstInt(_ dictionary: [String: Any], keys: [String]) -> Int64? {
        for key in keys {
            if let value = JSONSupport.int64(dictionary[key]) { return value }
        }
        return nil
    }

    private static func firstDouble(_ dictionary: [String: Any], keys: [String]) -> Double? {
        for key in keys {
            if let value = JSONSupport.number(dictionary[key]) { return value }
        }
        return nil
    }

    private static func firstDate(_ dictionary: [String: Any], keys: [String]) -> Date? {
        for key in keys {
            if let value = JSONSupport.date(dictionary[key]) { return value }
        }
        return nil
    }
}

private extension JSONSupport {
    static func string(_ value: Any?) -> String? {
        value as? String
    }
}
