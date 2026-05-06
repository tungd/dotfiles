import Foundation

struct EmacsServerProfile {
    var defaultFontSize: Double?
    var defaultFontFamily: String?
    var defaultForegroundColor: String?
    var defaultBackgroundColor: String?

    static let empty = EmacsServerProfile(
        defaultFontSize: nil,
        defaultFontFamily: nil,
        defaultForegroundColor: nil,
        defaultBackgroundColor: nil
    )
}
