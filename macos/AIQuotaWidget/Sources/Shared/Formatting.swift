import Foundation

enum QuotaFormat {
    static func compactTokens(_ value: Int64?) -> String {
        guard let value else { return "—" }
        let number = Double(value)
        if number >= 1_000_000 {
            return String(format: "%.1fM", number / 1_000_000)
        }
        if number >= 1_000 {
            return String(format: "%.1fK", number / 1_000)
        }
        return "\(value)"
    }

    static func percent(_ value: Double?) -> String {
        guard let value else { return "—" }
        return String(format: "%.0f%%", value)
    }

    static func percentagePoints(_ value: Double?) -> String {
        guard let value else { return "—" }
        return String(format: "%.0f pp", abs(value))
    }

    static func reset(_ date: Date?, now: Date = Date()) -> String {
        guard let date else { return "Reset unavailable" }
        let formatter = RelativeDateTimeFormatter()
        formatter.unitsStyle = .short
        return "Resets \(formatter.localizedString(for: date, relativeTo: now))"
    }

    static func dateTime(_ date: Date?) -> String {
        guard let date else { return "—" }
        let formatter = DateFormatter()
        formatter.dateStyle = .short
        formatter.timeStyle = .short
        return formatter.string(from: date)
    }

    static func lastUpdated(_ date: Date) -> String {
        guard date != .distantPast else { return "No data yet" }
        let formatter = RelativeDateTimeFormatter()
        formatter.unitsStyle = .short
        return "Updated \(formatter.localizedString(for: date, relativeTo: Date()))"
    }

    static func pace(_ value: Double?) -> String {
        guard let value else { return "Pace unavailable" }
        if abs(value) < 0.5 { return "On pace" }
        if value > 0 { return "Ahead of pace by \(percentagePoints(value))" }
        return "Behind pace by \(percentagePoints(value))"
    }

    static func tokenLine(_ window: WindowSnapshot) -> String {
        guard let used = window.usedTokens else { return "Token usage unavailable" }
        if let limit = window.limitTokens {
            return "\(compactTokens(used)) / \(compactTokens(limit)) tokens"
        }
        return "\(compactTokens(used)) tokens"
    }
}
