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

    func queryProfile() async -> EmacsServerProfile {
        let frameFont = try? await queryDefaultFrameFont()
        let parsedFrameFont = frameFont.flatMap(Self.parseFontSpec)
        var defaultFontSize = parsedFrameFont?.size
        if defaultFontSize == nil {
            defaultFontSize = try? await queryDefaultFaceFontSize()
        }

        var defaultFontFamily = parsedFrameFont?.family
        if defaultFontFamily == nil {
            defaultFontFamily = try? await queryDefaultFaceFontFamily()
        }

        return EmacsServerProfile(
            defaultFontSize: defaultFontSize,
            defaultFontFamily: defaultFontFamily,
            defaultForegroundColor: try? await queryDefaultFaceColor(attribute: "foreground"),
            defaultBackgroundColor: try? await queryDefaultFaceColor(attribute: "background")
        )
    }

    func installHostIntegration() async {
        _ = try? await Self.run(
            executable: config.emacsclientPath,
            arguments: ["-s", config.serverName, "-e", Self.hostIntegrationExpression]
        )
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

    private func queryDefaultFrameFont() async throws -> String? {
        let output = try await Self.run(
            executable: config.emacsclientPath,
            arguments: ["-s", config.serverName, "-e", "(alist-get 'font default-frame-alist)"]
        )
        let trimmed = output.trimmingCharacters(in: .whitespacesAndNewlines)
        guard trimmed != "nil", !trimmed.isEmpty else {
            return nil
        }
        return Self.parseElispString(trimmed)
    }

    private func queryDefaultFaceFontSize() async throws -> Double? {
        let expression = """
        (let ((height (face-attribute 'default :height nil 'default)))
          (if (and (integerp height) (>= height 50)) (/ height 10.0) nil))
        """
        let output = try await Self.run(
            executable: config.emacsclientPath,
            arguments: ["-s", config.serverName, "-e", expression]
        )
        let trimmed = output.trimmingCharacters(in: .whitespacesAndNewlines)
        guard trimmed != "nil", !trimmed.isEmpty else {
            return nil
        }
        return Double(trimmed)
    }

    private func queryDefaultFaceFontFamily() async throws -> String? {
        let expression = """
        (let ((family (face-attribute 'default :family nil 'default)))
          (if (and (stringp family) (not (member family '("default" "unspecified"))))
              family
            nil))
        """
        let output = try await Self.run(
            executable: config.emacsclientPath,
            arguments: ["-s", config.serverName, "-e", expression]
        )
        let trimmed = output.trimmingCharacters(in: .whitespacesAndNewlines)
        guard trimmed != "nil", !trimmed.isEmpty else {
            return nil
        }
        return Self.parseElispString(trimmed)
    }

    private func queryDefaultFaceColor(attribute: String) async throws -> String? {
        let expression = """
        (let ((color (face-attribute 'default :\(attribute) nil 'default)))
          (if (and (stringp color)
                   (not (member color '("default" "unspecified" "unspecified-fg" "unspecified-bg"))))
              color
            nil))
        """
        let output = try await Self.run(
            executable: config.emacsclientPath,
            arguments: ["-s", config.serverName, "-e", expression]
        )
        let trimmed = output.trimmingCharacters(in: .whitespacesAndNewlines)
        guard trimmed != "nil", !trimmed.isEmpty else {
            return nil
        }
        return Self.parseElispString(trimmed)
    }

    private static func parseFontSpec(_ fontSpec: String) -> (family: String?, size: Double?)? {
        let trimmed = fontSpec.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty, trimmed != "tty" else {
            return nil
        }

        let fontConfigPattern = #"(?i)^(.*):(?:size|pixelsize)=([0-9]+(?:\.[0-9]+)?)(?::.*)?$"#
        if let regex = try? NSRegularExpression(pattern: fontConfigPattern),
           let match = regex.firstMatch(
            in: trimmed,
            range: NSRange(trimmed.startIndex..<trimmed.endIndex, in: trimmed)
           ),
           let familyRange = Range(match.range(at: 1), in: trimmed),
           let sizeRange = Range(match.range(at: 2), in: trimmed) {
            let family = trimmed[familyRange]
                .split(separator: ":", maxSplits: 1)
                .first
                .map(String.init)?
                .trimmingCharacters(in: .whitespacesAndNewlines)
            return (
                family: family?.isEmpty == false ? family : nil,
                size: Double(trimmed[sizeRange])
            )
        }

        guard let range = trimmed.range(
            of: #"(?<=\S)[ -]+([0-9]+(?:\.[0-9]+)?)$"#,
            options: .regularExpression
        ) else {
            return (family: trimmed, size: nil)
        }

        let sizePart = trimmed[range]
            .trimmingCharacters(in: CharacterSet(charactersIn: " -"))
        let familyPart = trimmed[..<range.lowerBound]
            .trimmingCharacters(in: CharacterSet(charactersIn: " -"))

        return (
            family: familyPart.isEmpty ? nil : familyPart,
            size: Double(sizePart)
        )
    }

    private static func parseElispString(_ value: String) -> String? {
        guard value.first == "\"", value.last == "\"" else {
            return nil
        }

        if let data = value.data(using: .utf8),
           let decoded = try? JSONDecoder().decode(String.self, from: data) {
            return decoded
        }

        var result = ""
        var escaping = false
        for character in value.dropFirst().dropLast() {
            if escaping {
                switch character {
                case "n":
                    result.append("\n")
                case "r":
                    result.append("\r")
                case "t":
                    result.append("\t")
                default:
                    result.append(character)
                }
                escaping = false
            } else if character == "\\" {
                escaping = true
            } else {
                result.append(character)
            }
        }

        guard !escaping else {
            return nil
        }
        return result
    }

    private static let hostIntegrationExpression = #"""
    (progn
      (require 'server)
      (defvar emacs-host--last-title-by-terminal (make-hash-table :test 'eq))
      (defun emacs-host--clean-title (title)
        (replace-regexp-in-string "[\a\e]" "" (or title "")))
      (defun emacs-host--title-for-frame (frame)
        (with-selected-frame frame
          (emacs-host--clean-title (format-mode-line frame-title-format))))
      (defun emacs-host--emit-title (&optional frame)
        (let* ((frame (or frame (selected-frame)))
               (terminal (frame-terminal frame)))
          (when (and (terminal-live-p terminal)
                     (not (display-graphic-p frame)))
            (let ((title (emacs-host--title-for-frame frame)))
              (unless (equal title (gethash terminal emacs-host--last-title-by-terminal))
                (puthash terminal title emacs-host--last-title-by-terminal)
                (send-string-to-terminal
                 (concat "\e]777;emacs;tab-title;" title "\a")
                 terminal))))))
      (defun emacs-host--post-command-title ()
        (emacs-host--emit-title (selected-frame)))
      (defun emacs-host--setup-terminal-frame (&optional frame)
        (let ((frame (or frame (selected-frame))))
          (with-selected-frame frame
            (when (not (display-graphic-p frame))
              (when (fboundp 'xterm-mouse-mode)
                (condition-case nil
                    (xterm-mouse-mode 1)
                  (error nil)))
              (when (fboundp 'blink-cursor-mode)
                (blink-cursor-mode -1))
              (emacs-host--emit-title frame)))))
      (add-hook 'server-after-make-frame-hook #'emacs-host--setup-terminal-frame)
      (add-hook 'post-command-hook #'emacs-host--post-command-title)
      (emacs-host--setup-terminal-frame (selected-frame))
      'emacs-host-integration-installed)
    """#

    @discardableResult
    private static func run(executable: String, arguments: [String]) async throws -> String {
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
            let stdout = output.fileHandleForReading.readDataToEndOfFile()
            let stderr = error.fileHandleForReading.readDataToEndOfFile()

            guard process.terminationStatus == 0 else {
                let message = String(data: stderr + stdout, encoding: .utf8)?
                    .trimmingCharacters(in: .whitespacesAndNewlines)
                throw EmacsDaemonError.processFailed(
                    message?.isEmpty == false ? message! : "\(executable) exited \(process.terminationStatus)"
                )
            }

            return String(data: stdout, encoding: .utf8) ?? ""
        }.value
    }
}
