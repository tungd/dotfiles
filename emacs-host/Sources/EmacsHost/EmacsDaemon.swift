import Foundation

enum EmacsDaemonError: Error, CustomStringConvertible {
    case processFailed(String)
    case serverUnavailable(String)

    var description: String {
        switch self {
        case .processFailed(let message):
            message
        case .serverUnavailable(let message):
            message
        }
    }
}

actor EmacsDaemon {
    private let config: AppConfig
    private var startupTask: Task<Void, Error>?

    init(config: AppConfig) {
        self.config = config
    }

    func ensureStarted() async throws {
        if await isRunning() {
            return
        }

        if let startupTask {
            try await startupTask.value
            return
        }

        let task = Task {
            try await Self.run(
                executable: config.emacsPath,
                arguments: ["--daemon=\(config.serverName)"]
            )

            for _ in 0..<50 {
                if await self.isRunning() {
                    return
                }
                try await Task.sleep(nanoseconds: 100_000_000)
            }

            throw EmacsDaemonError.serverUnavailable(
                "Emacs server \(config.serverName) did not become available"
            )
        }

        startupTask = task
        do {
            try await task.value
            startupTask = nil
        } catch {
            startupTask = nil
            throw error
        }
    }

    private func isRunning() async -> Bool {
        do {
            try await Self.run(
                executable: config.emacsclientPath,
                arguments: ["-s", config.serverName, "-e", "t"]
            )
            return true
        } catch {
            return false
        }
    }

    private static func run(executable: String, arguments: [String]) async throws {
        try await Task.detached(priority: .utility) {
            let process = Process()
            process.executableURL = URL(fileURLWithPath: executable)
            process.arguments = arguments

            let output = Pipe()
            let error = Pipe()
            process.standardOutput = output
            process.standardError = error

            do {
                try process.run()
            } catch {
                throw EmacsDaemonError.processFailed("\(executable) failed to start: \(error)")
            }

            process.waitUntilExit()

            guard process.terminationStatus == 0 else {
                let stderr = error.fileHandleForReading.readDataToEndOfFile()
                let stdout = output.fileHandleForReading.readDataToEndOfFile()
                let message = String(data: stderr + stdout, encoding: .utf8)?
                    .trimmingCharacters(in: .whitespacesAndNewlines)
                throw EmacsDaemonError.processFailed(
                    message?.isEmpty == false ? message! : "\(executable) exited \(process.terminationStatus)"
                )
            }
        }.value
    }
}
