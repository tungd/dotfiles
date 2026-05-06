import Foundation

struct HostEvent {
    let command: String
    let payload: String
}

final class HostEventParser {
    private enum OSCHandling {
        case event(HostEvent)
        case consume
    }

    private var pending: [UInt8] = []

    func process(_ bytes: [UInt8]) -> (terminal: [UInt8], events: [HostEvent]) {
        pending.append(contentsOf: bytes)

        var output: [UInt8] = []
        var events: [HostEvent] = []
        var index = 0

        while index < pending.count {
            guard pending[index] == 0x1b else {
                output.append(pending[index])
                index += 1
                continue
            }

            guard index + 1 < pending.count else {
                break
            }

            guard pending[index + 1] == 0x5d else {
                output.append(pending[index])
                index += 1
                continue
            }

            guard let terminator = findOSCTerminator(startingAt: index + 2) else {
                if pending.count - index > 65_536 {
                    output.append(contentsOf: pending[index...])
                    index = pending.count
                }
                break
            }

            let content = Array(pending[(index + 2)..<terminator.start])
            switch parseOSC(content) {
            case .event(let event):
                events.append(event)
            case .consume:
                break
            case nil:
                output.append(contentsOf: pending[index..<terminator.end])
            }
            index = terminator.end
        }

        pending.removeFirst(index)
        return (output, events)
    }

    private func findOSCTerminator(startingAt start: Int) -> (start: Int, end: Int)? {
        var index = start
        while index < pending.count {
            if pending[index] == 0x07 {
                return (index, index + 1)
            }
            if pending[index] == 0x1b,
               index + 1 < pending.count,
               pending[index + 1] == 0x5c {
                return (index, index + 2)
            }
            index += 1
        }
        return nil
    }

    private func parseOSC(_ content: [UInt8]) -> OSCHandling? {
        guard let text = String(bytes: content, encoding: .utf8) else {
            return nil
        }

        if let event = parseHostEvent(text) {
            return .event(event)
        }

        if text.hasPrefix("52;") {
            return parseClipboardEvent(text) ?? .consume
        }

        return nil
    }

    private func parseHostEvent(_ text: String) -> HostEvent? {
        let parts = text.split(separator: ";", maxSplits: 3, omittingEmptySubsequences: false)
        guard parts.count >= 3,
              parts[0] == "777",
              parts[1] == "emacs" else {
            return nil
        }

        return HostEvent(
            command: String(parts[2]),
            payload: parts.count >= 4 ? String(parts[3]) : ""
        )
    }

    private func parseClipboardEvent(_ text: String) -> OSCHandling? {
        let parts = text.split(separator: ";", maxSplits: 2, omittingEmptySubsequences: false)
        guard parts.count == 3,
              parts[0] == "52" else {
            return nil
        }

        let target = String(parts[1])
        let encodedPayload = String(parts[2])
        guard isClipboardTargetSupported(target) else {
            return .consume
        }

        if encodedPayload == "?" {
            let payload = target.isEmpty ? "c" : target
            return .event(HostEvent(command: "clipboard-read", payload: payload))
        }

        guard let data = Data(base64Encoded: encodedPayload),
              let payload = String(data: data, encoding: .utf8) else {
            return nil
        }

        return .event(HostEvent(command: "clipboard-write", payload: payload))
    }

    private func isClipboardTargetSupported(_ target: String) -> Bool {
        target.isEmpty || target.allSatisfy { "cps".contains($0) }
    }
}
