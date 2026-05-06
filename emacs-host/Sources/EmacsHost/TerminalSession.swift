import Foundation

@MainActor
protocol TerminalSessionDelegate: AnyObject {
    func terminalSession(_ session: TerminalSession, didReceive bytes: [UInt8])
    func terminalSession(_ session: TerminalSession, didReceive event: HostEvent)
    func terminalSessionDidExit(_ session: TerminalSession)
}

final class TerminalSession {
    let id = UUID()
    let title: String

    private let pty: PTYProcess
    private let parser = HostEventParser()
    private var readTask: Task<Void, Never>?

    @MainActor weak var delegate: TerminalSessionDelegate?

    init(config: AppConfig, index: Int) throws {
        title = "Emacs \(index)"
        let argv = [
            "/usr/bin/env",
            "TERM=xterm-256color",
            "COLORTERM=truecolor",
            "EMACS_HOST=1",
            "EMACS_HOST_OSC=777",
            config.emacsclientPath,
            "-t",
            "-s",
            config.serverName,
        ]

        pty = try PTYProcess(argv: argv, rows: config.initialRows, cols: config.initialCols)
        pty.resize(rows: config.initialRows, cols: config.initialCols)

        readTask = Task { [weak self] in
            guard let self else { return }
            await pty.readLoop { [weak self] bytes in
                guard let self else { return }
                let parsed = self.parser.process(bytes)
                await MainActor.run {
                    if !parsed.terminal.isEmpty {
                        self.delegate?.terminalSession(self, didReceive: parsed.terminal)
                    }
                    for event in parsed.events {
                        self.delegate?.terminalSession(self, didReceive: event)
                    }
                }
            }

            await MainActor.run {
                self.delegate?.terminalSessionDidExit(self)
            }
        }
    }

    func send(_ bytes: [UInt8]) {
        try? pty.write(bytes)
    }

    func resize(rows: Int, cols: Int) {
        pty.resize(rows: rows, cols: cols)
    }

    func close() {
        readTask?.cancel()
        pty.terminate()
    }

    deinit {
        close()
    }
}
