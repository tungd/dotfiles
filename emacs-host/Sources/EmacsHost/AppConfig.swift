import Foundation

struct AppConfig {
    let emacsPath: String
    let emacsclientPath: String
    let serverName: String
    let initialRows: Int
    let initialCols: Int

    static func load() -> AppConfig {
        let environment = ProcessInfo.processInfo.environment
        let emacsPath = environment["EMACS_HOST_EMACS"].flatMap(nonEmpty)
            ?? findExecutable("emacs")
            ?? "/usr/bin/emacs"
        let emacsclientPath = environment["EMACS_HOST_EMACSCLIENT"].flatMap(nonEmpty)
            ?? findExecutable("emacsclient")
            ?? "/usr/bin/emacsclient"
        let serverName = environment["EMACS_HOST_SERVER"].flatMap(nonEmpty)
            ?? "emacs-host"

        return .init(
            emacsPath: emacsPath,
            emacsclientPath: emacsclientPath,
            serverName: serverName,
            initialRows: 40,
            initialCols: 120
        )
    }

    private static func nonEmpty(_ value: String) -> String? {
        value.isEmpty ? nil : value
    }

    private static func findExecutable(_ name: String) -> String? {
        let pathCandidates = (ProcessInfo.processInfo.environment["PATH"] ?? "")
            .split(separator: ":")
            .map(String.init)
            .map { "\($0)/\(name)" }

        let fixedCandidates = [
            "/Users/tung/.local/bin/\(name)",
            "/opt/local/bin/\(name)",
            "/opt/homebrew/bin/\(name)",
            "/usr/local/bin/\(name)",
            "/usr/bin/\(name)",
        ]

        return (pathCandidates + fixedCandidates)
            .first { FileManager.default.isExecutableFile(atPath: $0) }
    }
}
