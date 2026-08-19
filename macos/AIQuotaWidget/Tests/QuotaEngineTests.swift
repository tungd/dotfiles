import Foundation
import XCTest

final class QuotaEngineTests: XCTestCase {
    func testClaudeUsageProducesHeadroomAndPace() throws {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("AIQuotaWidgetTests-\(UUID().uuidString)", isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: directory) }

        let now = Date()
        let timestamp = ISO8601DateFormatter().string(from: now.addingTimeInterval(-60))
        let line = "{\"timestamp\":\"\(timestamp)\",\"usage\":{\"input_tokens\":100,\"output_tokens\":200}}\n"
        let file = directory.appendingPathComponent("session.jsonl")
        try line.data(using: .utf8)?.write(to: file)

        let config = WidgetConfig(
            providers: [
                ProviderConfig(
                    id: "claude",
                    name: "Claude",
                    source: .claude,
                    paths: [file.path],
                    windows: [QuotaWindowConfig(id: "5h", label: "5h", minutes: 300, limitTokens: 1_000)]
                )
            ]
        )

        let snapshot = QuotaEngine(config: config).collect(now: now)
        let window = try XCTUnwrap(snapshot.providers.first?.windows.first)
        XCTAssertEqual(window.usedTokens, 300)
        XCTAssertEqual(window.usedPercent ?? 0, 30, accuracy: 0.01)
        XCTAssertEqual(window.headroomPercent ?? 0, 70, accuracy: 0.01)
        XCTAssertEqual(snapshot.providers.first?.status, "ready")
    }

    func testGenericUsageFileSupportsWeeklyPercent() throws {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("AIQuotaWidgetTests-\(UUID().uuidString)", isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: directory) }

        let file = directory.appendingPathComponent("usage.json")
        let json = """
        {"windows":{"weekly":{"usedPercent":42,"resetAt":"2030-01-01T00:00:00Z"}}}
        """
        try json.data(using: .utf8)?.write(to: file)

        let config = WidgetConfig(
            providers: [
                ProviderConfig(
                    id: "hf",
                    name: "HF Pro Inference",
                    source: .generic,
                    paths: [file.path],
                    windows: [QuotaWindowConfig(id: "weekly", label: "Weekly", minutes: 10080)]
                )
            ]
        )

        let snapshot = QuotaEngine(config: config).collect(now: Date())
        let window = try XCTUnwrap(snapshot.providers.first?.windows.first)
        XCTAssertEqual(window.usedPercent ?? 0, 42, accuracy: 0.01)
        XCTAssertEqual(window.headroomPercent ?? 0, 58, accuracy: 0.01)
    }

    func testClaudeLocalQuotaCacheProvidesWindowPercentAndReset() throws {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("AIQuotaWidgetTests-\(UUID().uuidString)", isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: directory) }

        let file = directory.appendingPathComponent(".claude.json")
        let json = """
        {"cachedUsageUtilization":{"utilization":{"five_hour":{"utilization":34,"resets_at":"2030-01-01T00:00:00Z"},"seven_day":{"utilization":87,"resets_at":"2030-01-02T00:00:00Z"}}}}
        """
        try json.data(using: .utf8)?.write(to: file)

        let config = WidgetConfig(
            providers: [
                ProviderConfig(
                    id: "claude",
                    name: "Claude",
                    source: .claude,
                    paths: [file.path],
                    windows: [
                        QuotaWindowConfig(id: "5h", label: "5h", minutes: 300),
                        QuotaWindowConfig(id: "weekly", label: "Weekly", minutes: 10080)
                    ]
                )
            ]
        )

        let snapshot = QuotaEngine(config: config).collect(now: Date())
        let windows = try XCTUnwrap(snapshot.providers.first?.windows)
        XCTAssertEqual(windows[0].usedPercent ?? 0, 34, accuracy: 0.01)
        XCTAssertEqual(windows[1].usedPercent ?? 0, 87, accuracy: 0.01)
        XCTAssertNotNil(windows[1].resetAt)
        XCTAssertEqual(snapshot.providers.first?.status, "ready")
    }

    func testCodexLegacyRateLimitNormalizesFiveHourWindowAndCountdown() throws {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("AIQuotaWidgetTests-\(UUID().uuidString)", isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: directory) }

        let file = directory.appendingPathComponent("rollout.jsonl")
        let now = Date()
        let timestamp = ISO8601DateFormatter().string(from: now)
        let json = """
        {"timestamp":"\(timestamp)","payload":{"rate_limits":{"primary":{"used_percent":34,"window_minutes":299,"resets_in_seconds":7200}}}}
        """
        try json.data(using: .utf8)?.write(to: file)

        let result = CodexImporter.scan(paths: [file.path])
        let value = try XCTUnwrap(result.values["5h"])
        XCTAssertEqual(value.windowMinutes, 300)
        XCTAssertEqual(value.usedPercent, 34, accuracy: 0.01)
        XCTAssertEqual(value.resetAt?.timeIntervalSince(now) ?? 0, 7200, accuracy: 1)
    }

    func testHuggingFaceDailyQuotaUsesRemainingSecondsAndReset() throws {
        let resetAt = "2030-01-01T00:00:00Z"
        let value = try XCTUnwrap(HFImporter.parseDailyQuota(from: [
            "zeroGpu": [
                "base": 2400,
                "current": 1800,
                "resetsAt": resetAt
            ]
        ]))

        XCTAssertEqual(value.windowMinutes, 1440)
        XCTAssertEqual(value.usedPercent, 25, accuracy: 0.01)
        XCTAssertNotNil(value.resetAt)
    }

    func testWidgetSnapshotFollowsCurrentConfigWhenCachedSnapshotIsStale() throws {
        let staleSnapshot = DashboardSnapshot(
            generatedAt: Date(),
            providers: [
                ProviderSnapshot(
                    id: "huggingface",
                    name: "HF Pro Inference",
                    source: .generic,
                    status: "needs-setup",
                    windows: [
                        WindowSnapshot(
                            id: "5h",
                            label: "5h",
                            windowMinutes: 300,
                            usedTokens: nil,
                            limitTokens: nil,
                            usedPercent: nil,
                            headroomPercent: nil,
                            resetAt: nil,
                            burnRateTokensPerHour: nil,
                            burnRatePercentPerHour: nil,
                            paceDeltaPercentagePoints: nil,
                            sourceNote: "No local data"
                        ),
                        WindowSnapshot(
                            id: "weekly",
                            label: "Weekly",
                            windowMinutes: 10080,
                            usedTokens: nil,
                            limitTokens: nil,
                            usedPercent: nil,
                            headroomPercent: nil,
                            resetAt: nil,
                            burnRateTokensPerHour: nil,
                            burnRatePercentPerHour: nil,
                            paceDeltaPercentagePoints: nil,
                            sourceNote: "No local data"
                        )
                    ],
                    detail: "No usage/quota JSON file found."
                )
            ],
            deepSeek: DashboardSnapshot.empty.deepSeek
        )
        let config = WidgetConfig(
            providers: [
                ProviderConfig(
                    id: "huggingface",
                    name: "HF Pro Inference",
                    source: .huggingface,
                    paths: [],
                    windows: [QuotaWindowConfig(id: "daily", label: "Daily", minutes: 1440)]
                )
            ]
        )

        let aligned = SnapshotPresentation.aligned(staleSnapshot, to: config)
        let provider = try XCTUnwrap(aligned.providers.first)
        XCTAssertEqual(provider.source, .huggingface)
        XCTAssertEqual(provider.windows.map(\.id), ["daily"])
        XCTAssertEqual(provider.status, "needs-setup")
    }

    func testDeepSeekSupportsTwoPublishedPeakWindows() {
        let snapshot = DeepSeekCalculator.snapshot(
            schedule: DeepSeekSchedule(
                timezone: "Asia/Ho_Chi_Minh",
                peakWindows: [
                    DeepSeekPeakWindow(start: "08:00", end: "11:00"),
                    DeepSeekPeakWindow(start: "13:00", end: "17:00")
                ]
            ),
            now: Date()
        )
        XCTAssertTrue(snapshot.windowDescription.contains("08:00–11:00 + 13:00–17:00"))
    }

    func testDeepSeekScheduleReportsUnconfiguredWithoutInventingWindow() {
        let snapshot = DeepSeekCalculator.snapshot(
            schedule: DeepSeekSchedule(timezone: "Asia/Ho_Chi_Minh"),
            now: Date()
        )
        XCTAssertEqual(snapshot.state, .notConfigured)
        XCTAssertNil(snapshot.nextChangeAt)
    }

    func testMissingSourceDoesNotBecomeZeroUsage() throws {
        let config = WidgetConfig(
            providers: [
                ProviderConfig(
                    id: "hf",
                    name: "HF Pro Inference",
                    source: .generic,
                    paths: ["/tmp/ai-quota-widget-no-such-file.json"],
                    windows: [QuotaWindowConfig(id: "5h", label: "5h", minutes: 300, limitTokens: 1_000)]
                )
            ]
        )

        let snapshot = QuotaEngine(config: config).collect(now: Date())
        let window = try XCTUnwrap(snapshot.providers.first?.windows.first)
        XCTAssertNil(window.usedTokens)
        XCTAssertNil(window.usedPercent)
        XCTAssertEqual(snapshot.providers.first?.status, "needs-setup")
    }
}
