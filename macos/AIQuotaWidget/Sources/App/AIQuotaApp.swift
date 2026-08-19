import SwiftUI

@main
struct AIQuotaApp: App {
    @StateObject private var model = AppModel()
    @Environment(\.openWindow) private var openWindow

    var body: some Scene {
        Window("Quota", id: "dashboard") {
            DashboardView(model: model)
                .onOpenURL { url in
                    guard url.scheme == "aiquotawidget" else { return }
                    openWindow(id: "dashboard")
                }
        }
        .defaultSize(width: 620, height: 720)

        Window("Quota Settings", id: "settings") {
            SettingsView(model: model)
        }
        .defaultSize(width: 720, height: 680)

        MenuBarExtra("Quota", systemImage: "gauge.with.dots.needle.67percent") {
            MenuBarView(model: model)
        }
    }
}
