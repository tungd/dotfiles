import Foundation

enum DeepSeekCalculator {
    static func snapshot(schedule: DeepSeekSchedule, now: Date) -> DeepSeekSnapshot {
        guard let timeZone = TimeZone(identifier: schedule.timezone) else {
            return DeepSeekSnapshot(
                state: .notConfigured,
                timezone: schedule.timezone,
                windowDescription: "Peak window not configured",
                nextChangeAt: nil,
                detail: "Set peakWindows in Settings."
            )
        }

        var calendar = Calendar(identifier: .gregorian)
        calendar.timeZone = timeZone
        let allowedDays = Set(schedule.days ?? Array(1...7))
        let windows = schedule.peakWindows.compactMap { window -> (start: Int, end: Int)? in
            guard let start = parseMinutes(window.start),
                  let end = parseMinutes(window.end) else { return nil }
            return (start, end)
        }
        guard !windows.isEmpty else {
            return DeepSeekSnapshot(
                state: .notConfigured,
                timezone: schedule.timezone,
                windowDescription: "Peak window not configured",
                nextChangeAt: nil,
                detail: "Set peakWindows in Settings."
            )
        }

        let current = windows.flatMap { window in
            occurrences(
                startMinutes: window.start,
                endMinutes: window.end,
                calendar: calendar,
                allowedDays: allowedDays,
                around: now
            )
        }.sorted { $0.start < $1.start }
        let activeEnds = current.filter { now >= $0.start && now < $0.end }.map { $0.end }
        let active = !activeEnds.isEmpty
        let nextChange = active
            ? activeEnds.min()
            : current.first(where: { $0.start > now })?.start
        let formatter = DateFormatter()
        formatter.calendar = calendar
        formatter.timeZone = timeZone
        formatter.dateFormat = "HH:mm"
        let description = windows.map { window in
            "\(formatter.string(from: dateFor(minutes: window.start, on: now, calendar: calendar)))–\(formatter.string(from: dateFor(minutes: window.end, on: now, calendar: calendar)))"
        }.joined(separator: " + ") + " \(schedule.timezone)"

        return DeepSeekSnapshot(
            state: active ? .peak : .outsidePeak,
            timezone: schedule.timezone,
            windowDescription: description,
            nextChangeAt: nextChange,
            detail: active ? "Inside configured peak hours." : "Outside configured peak hours."
        )
    }

    private struct Occurrence {
        var start: Date
        var end: Date
    }

    private static func occurrences(
        startMinutes: Int,
        endMinutes: Int,
        calendar: Calendar,
        allowedDays: Set<Int>,
        around now: Date
    ) -> [Occurrence] {
        var result: [Occurrence] = []
        let today = calendar.startOfDay(for: now)
        let crossesMidnight = endMinutes <= startMinutes

        for dayOffset in -1...8 {
            guard let day = calendar.date(byAdding: .day, value: dayOffset, to: today),
                  allowedDays.contains(calendar.component(.weekday, from: day)) else { continue }
            let start = dateFor(minutes: startMinutes, on: day, calendar: calendar)
            let endDay = crossesMidnight
                ? (calendar.date(byAdding: .day, value: 1, to: day) ?? day)
                : day
            let end = dateFor(minutes: endMinutes, on: endDay, calendar: calendar)
            result.append(Occurrence(start: start, end: end))
        }

        return result.sorted { $0.start < $1.start }
    }

    private static func dateFor(minutes: Int, on date: Date, calendar: Calendar) -> Date {
        calendar.date(
            bySettingHour: minutes / 60,
            minute: minutes % 60,
            second: 0,
            of: date
        ) ?? date
    }

    private static func parseMinutes(_ value: String?) -> Int? {
        guard let value else { return nil }
        let parts = value.split(separator: ":")
        guard parts.count == 2,
              let hours = Int(parts[0]),
              let minutes = Int(parts[1]),
              (0..<24).contains(hours),
              (0..<60).contains(minutes) else { return nil }
        return hours * 60 + minutes
    }
}
