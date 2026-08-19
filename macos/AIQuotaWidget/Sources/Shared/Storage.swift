import Foundation

enum SnapshotStore {
    static let appName = "AIQuotaWidget"
    static let appGroupIdentifier = "P2FJWTSN96.group.com.tung.aiquotawidget"

    static var applicationSupportDirectory: URL {
        if let sharedContainer = FileManager.default.containerURL(
            forSecurityApplicationGroupIdentifier: appGroupIdentifier
        ) {
            return sharedContainer.appendingPathComponent(appName, isDirectory: true)
        }

        return legacyApplicationSupportDirectory
    }

    private static var legacyApplicationSupportDirectory: URL {
        let base = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
            ?? URL(fileURLWithPath: NSHomeDirectory()).appendingPathComponent("Library/Application Support")
        return base.appendingPathComponent(appName, isDirectory: true)
    }

    static var snapshotURL: URL {
        applicationSupportDirectory.appendingPathComponent("snapshot.json")
    }

    static var configURL: URL {
        let base = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".config", isDirectory: true)
            .appendingPathComponent("ai-quota-widget", isDirectory: true)
        return base.appendingPathComponent("config.json")
    }

    static func loadConfig() -> WidgetConfig {
        guard let data = try? Data(contentsOf: configURL) else {
            return DefaultConfig.value
        }

        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        return (try? decoder.decode(WidgetConfig.self, from: data)) ?? DefaultConfig.value
    }

    static func saveConfig(_ config: WidgetConfig) throws {
        let directory = configURL.deletingLastPathComponent()
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)

        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        try encoder.encode(config).write(to: configURL, options: .atomic)
    }

    static func saveSnapshot(_ snapshot: DashboardSnapshot) throws {
        try FileManager.default.createDirectory(
            at: applicationSupportDirectory,
            withIntermediateDirectories: true
        )

        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        try encoder.encode(snapshot).write(to: snapshotURL, options: .atomic)
    }

    static func loadSnapshot() -> DashboardSnapshot {
        let urls = [snapshotURL, legacyApplicationSupportDirectory.appendingPathComponent("snapshot.json")]
        for url in urls {
            guard let data = try? Data(contentsOf: url) else { continue }

            let decoder = JSONDecoder()
            decoder.dateDecodingStrategy = .iso8601
            if let snapshot = try? decoder.decode(DashboardSnapshot.self, from: data) {
                return snapshot
            }
        }

        return .empty
    }

    static func expandPath(_ path: String) -> URL {
        let expanded = NSString(string: path).expandingTildeInPath
        return URL(fileURLWithPath: expanded).standardizedFileURL
    }
}
