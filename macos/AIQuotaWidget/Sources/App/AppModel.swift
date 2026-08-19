import Foundation
import Combine
import AppKit
import WidgetKit

@MainActor
final class AppModel: ObservableObject {
    @Published private(set) var snapshot: DashboardSnapshot
    @Published private(set) var isRefreshing = false
    @Published private(set) var lastError: String?
    @Published var configText: String

    private(set) var config: WidgetConfig
    private var refreshLoop: Task<Void, Never>?

    init() {
        config = SnapshotStore.loadConfig()
        snapshot = SnapshotPresentation.aligned(SnapshotStore.loadSnapshot(), to: config)
        configText = Self.encode(config)
        refresh()
        startRefreshLoop()
    }

    deinit {
        refreshLoop?.cancel()
    }

    func refresh() {
        guard !isRefreshing else { return }
        isRefreshing = true
        lastError = nil
        let currentConfig = config

        Task { [weak self] in
            let result = await Task.detached(priority: .utility) {
                QuotaEngine(config: currentConfig).collect()
            }.value

            guard let self else { return }
            do {
                try SnapshotStore.saveSnapshot(result)
                snapshot = result
            } catch {
                lastError = "Could not save the local snapshot: \(error.localizedDescription)"
            }
            isRefreshing = false
            WidgetCenter.shared.reloadTimelines(ofKind: "com.tung.aiquotawidget.status.v2")
            WidgetCenter.shared.reloadAllTimelines()
        }
    }

    func saveConfig() {
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        guard let data = configText.data(using: .utf8),
              let decoded = try? decoder.decode(WidgetConfig.self, from: data) else {
            lastError = "Settings are not valid JSON."
            return
        }

        let normalized = SnapshotStore.normalizedConfig(decoded)
        do {
            try SnapshotStore.saveConfig(normalized)
            config = normalized
            configText = Self.encode(normalized)
            lastError = nil
            refresh()
        } catch {
            lastError = "Could not save settings: \(error.localizedDescription)"
        }
    }

    func resetConfigText() {
        configText = Self.encode(DefaultConfig.value)
        lastError = nil
    }

    func openConfigFolder() {
        NSWorkspace.shared.open(SnapshotStore.configURL.deletingLastPathComponent())
    }

    private func startRefreshLoop() {
        refreshLoop = Task { [weak self] in
            while !Task.isCancelled {
                let minutes = max(1, self?.config.refreshMinutes ?? 10)
                try? await Task.sleep(nanoseconds: UInt64(minutes) * 60 * 1_000_000_000)
                guard !Task.isCancelled else { return }
                await self?.refresh()
            }
        }
    }

    private static func encode(_ config: WidgetConfig) -> String {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        guard let data = try? encoder.encode(config) else { return DefaultConfig.json }
        return String(data: data, encoding: .utf8) ?? DefaultConfig.json
    }
}
