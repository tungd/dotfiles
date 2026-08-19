import Foundation

enum SnapshotPresentation {
    static func aligned(_ snapshot: DashboardSnapshot, to config: WidgetConfig) -> DashboardSnapshot {
        let existingProviders = Dictionary(
            uniqueKeysWithValues: snapshot.providers.map { ($0.id, $0) }
        )

        let providers = config.providers
            .filter(\.enabled)
            .map { providerConfig in
                let existing = existingProviders[providerConfig.id]
                let windows = providerConfig.windows.map { windowConfig in
                    let cached = existing?.windows.first {
                        $0.id == windowConfig.id || $0.windowMinutes == windowConfig.minutes
                    }

                    if let cached {
                        return WindowSnapshot(
                            id: windowConfig.id,
                            label: windowConfig.label,
                            windowMinutes: windowConfig.minutes,
                            usedTokens: cached.usedTokens,
                            limitTokens: cached.limitTokens ?? windowConfig.limitTokens,
                            usedPercent: cached.usedPercent,
                            headroomPercent: cached.headroomPercent,
                            resetAt: cached.resetAt ?? windowConfig.resetAt,
                            burnRateTokensPerHour: cached.burnRateTokensPerHour,
                            burnRatePercentPerHour: cached.burnRatePercentPerHour,
                            paceDeltaPercentagePoints: cached.paceDeltaPercentagePoints,
                            sourceNote: cached.sourceNote
                        )
                    }

                    return WindowSnapshot(
                        id: windowConfig.id,
                        label: windowConfig.label,
                        windowMinutes: windowConfig.minutes,
                        usedTokens: nil,
                        limitTokens: windowConfig.limitTokens,
                        usedPercent: nil,
                        headroomPercent: nil,
                        resetAt: windowConfig.resetAt,
                        burnRateTokensPerHour: nil,
                        burnRatePercentPerHour: nil,
                        paceDeltaPercentagePoints: nil,
                        sourceNote: "Waiting for app refresh"
                    )
                }

                let hasData = windows.contains(where: \.hasData)
                return ProviderSnapshot(
                    id: providerConfig.id,
                    name: providerConfig.name,
                    source: providerConfig.source,
                    status: hasData ? (existing?.status ?? "data-only") : "needs-setup",
                    windows: windows,
                    detail: hasData
                        ? (existing?.detail ?? "Quota data available.")
                        : "Waiting for the app to refresh quota data."
                )
            }

        return DashboardSnapshot(
            generatedAt: snapshot.generatedAt,
            providers: providers,
            deepSeek: snapshot.deepSeek
        )
    }
}
