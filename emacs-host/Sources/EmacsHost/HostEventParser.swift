import Foundation

struct HostEvent {
    let command: String
    let payload: String
}

final class HostEventParser {
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
            if let event = parseEvent(content) {
                events.append(event)
            } else {
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

    private func parseEvent(_ content: [UInt8]) -> HostEvent? {
        guard let text = String(bytes: content, encoding: .utf8) else {
            return nil
        }

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
}
