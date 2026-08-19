import Foundation

enum HFImporter {
    struct Result: Sendable {
        var values: [String: LiveQuotaValue]
        var detail: String
    }

    private static let usageURL = "https://huggingface.co/api/settings/billing/usage/live"

    static func scan(paths: [String]) -> Result {
        guard let token = token(paths: paths) else {
            return Result(
                values: [:],
                detail: "HF CLI token not found in the configured auth locations."
            )
        }

        guard let data = fetchUsageData(token: token),
              let object = firstSSEObject(from: data) else {
            return Result(
                values: [:],
                detail: "HF authenticated, but the daily quota endpoint returned no data."
            )
        }

        guard let daily = parseDailyQuota(from: object) else {
            return Result(
                values: [:],
                detail: "HF response did not include a daily ZeroGPU quota."
            )
        }

        return Result(
            values: ["daily": daily],
            detail: "Reads the authenticated HF daily quota."
        )
    }

    static func parseDailyQuota(from object: [String: Any]) -> LiveQuotaValue? {
        let zeroGPU = (object["zeroGpu"] as? [String: Any])
            ?? (object["zeroGPU"] as? [String: Any])
        guard let zeroGPU,
              let base = JSONSupport.number(zeroGPU["base"]),
              let current = JSONSupport.number(zeroGPU["current"]),
              base > 0 else {
            return nil
        }

        let usedPercent = min(max((base - current) / base * 100, 0), 100)
        return LiveQuotaValue(
            windowMinutes: 1440,
            usedPercent: usedPercent,
            resetAt: JSONSupport.date(zeroGPU["resetsAt"])
                ?? JSONSupport.date(zeroGPU["resets_at"]),
            note: "HF authenticated daily rate limit"
        )
    }

    private static func token(paths: [String]) -> String? {
        let environment = ProcessInfo.processInfo.environment
        var candidates: [URL] = []

        if let environmentToken = environment["HF_TOKEN"]?.trimmingCharacters(in: .whitespacesAndNewlines),
           !environmentToken.isEmpty {
            return environmentToken
        }

        if let home = environment["HF_HOME"], !home.isEmpty {
            candidates.append(SnapshotStore.expandPath(home).appendingPathComponent("token"))
        }

        for path in paths {
            let url = SnapshotStore.expandPath(path)
            var isDirectory: ObjCBool = false
            guard FileManager.default.fileExists(atPath: url.path, isDirectory: &isDirectory) else {
                continue
            }
            candidates.append(
                isDirectory.boolValue ? url.appendingPathComponent("token") : url
            )
        }

        let home = FileManager.default.homeDirectoryForCurrentUser
        candidates.append(home.appendingPathComponent(".cache/huggingface/token"))
        candidates.append(home.appendingPathComponent(".huggingface/token"))

        for url in candidates {
            guard let value = try? String(contentsOf: url, encoding: .utf8) else { continue }
            let token = value.trimmingCharacters(in: .whitespacesAndNewlines)
            if !token.isEmpty { return token }
        }
        return nil
    }

    private static func fetchUsageData(token: String) -> Data? {
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/curl")
        process.arguments = ["--config", "-"]

        let input = Pipe()
        let output = Pipe()
        process.standardInput = input
        process.standardOutput = output
        process.standardError = Pipe()

        let escapedToken = token
            .replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")
        let curlConfig = """
        url = "\(usageURL)"
        silent
        show-error
        no-buffer
        max-time = 5
        header = "Accept: text/event-stream"
        header = "Authorization: Bearer \(escapedToken)"
        """

        do {
            try process.run()
            try input.fileHandleForWriting.write(contentsOf: Data(curlConfig.utf8))
            try input.fileHandleForWriting.close()
            process.waitUntilExit()
        } catch {
            return nil
        }

        let data = output.fileHandleForReading.readDataToEndOfFile()
        return data.isEmpty ? nil : data
    }

    private static func firstSSEObject(from data: Data) -> [String: Any]? {
        guard let text = String(data: data, encoding: .utf8) else { return nil }
        var eventData = ""

        for line in text.components(separatedBy: .newlines) {
            if line.hasPrefix("data:") {
                let payload = line.dropFirst(5).trimmingCharacters(in: .whitespaces)
                eventData += payload
            } else if eventData.isEmpty == false {
                if let object = jsonObject(from: eventData) { return object }
                eventData = ""
            }
        }

        return jsonObject(from: eventData)
    }

    private static func jsonObject(from text: String) -> [String: Any]? {
        guard let data = text.data(using: .utf8),
              let object = try? JSONSerialization.jsonObject(with: data) else {
            return nil
        }
        return object as? [String: Any]
    }
}
