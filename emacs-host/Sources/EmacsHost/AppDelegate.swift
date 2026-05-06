import AppKit

@MainActor
final class AppDelegate: NSObject, NSApplicationDelegate {
    private let config = AppConfig.load()
    private lazy var daemon = EmacsDaemon(config: config)
    private lazy var rootController = RootViewController(config: config)
    private var window: NSWindow?
    private var titlebarLayout: WarpTitlebarLayout?

    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.setActivationPolicy(.regular)
        NSApp.activate(ignoringOtherApps: true)
        installMinimalMenu()
        makeWindow()

        Task {
            do {
                try await daemon.ensureStarted()
                await MainActor.run {
                    self.rootController.openNewTab(nil)
                }
            } catch {
                await MainActor.run {
                    self.showStartupError(error)
                }
            }
        }
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        true
    }

    private func makeWindow() {
        rootController.view.frame = NSRect(x: 0, y: 0, width: 1100, height: 760)

        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1100, height: 760),
            styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        window.title = "Emacs Host"
        window.titleVisibility = .hidden
        window.titlebarAppearsTransparent = true
        window.isMovableByWindowBackground = true
        window.contentViewController = rootController

        let titlebarLayout = WarpTitlebarLayout(window: window)
        self.titlebarLayout = titlebarLayout

        window.center()
        window.makeKeyAndOrderFront(nil)
        titlebarLayout.apply()
        DispatchQueue.main.async { [weak titlebarLayout] in
            titlebarLayout?.apply()
        }

        self.window = window
    }

    private func installMinimalMenu() {
        let mainMenu = NSMenu()
        let appMenuItem = NSMenuItem()
        mainMenu.addItem(appMenuItem)

        let appMenu = NSMenu()
        appMenu.addItem(
            withTitle: "Quit Emacs Host",
            action: #selector(NSApplication.terminate(_:)),
            keyEquivalent: ""
        )
        appMenuItem.submenu = appMenu

        let fileMenuItem = NSMenuItem()
        mainMenu.addItem(fileMenuItem)
        let fileMenu = NSMenu(title: "File")
        fileMenu.addItem(
            withTitle: "New Emacs Tab",
            action: #selector(RootViewController.openNewTab(_:)),
            keyEquivalent: ""
        )
        fileMenuItem.submenu = fileMenu

        NSApp.mainMenu = mainMenu
    }

    private func showStartupError(_ error: Error) {
        let alert = NSAlert()
        alert.alertStyle = .critical
        alert.messageText = "Emacs Host failed"
        alert.informativeText = String(describing: error)
        alert.runModal()
    }
}
