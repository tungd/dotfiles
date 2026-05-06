import AppKit

@MainActor
private final class EmacsHostWindow: NSWindow {
    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        if event.type == .keyDown,
           event.modifierFlags.contains(.command),
           let rootController = contentViewController as? RootViewController,
           rootController.performHostKeyEquivalent(with: event) {
            return true
        }

        return super.performKeyEquivalent(with: event)
    }
}

@MainActor
final class AppDelegate: NSObject, NSApplicationDelegate {
    private let config = AppConfig.load()
    private lazy var daemon = EmacsDaemon(config: config)
    private lazy var rootController = RootViewController(config: config)
    private var window: NSWindow?
    private var titlebarLayout: WarpTitlebarLayout?
    private var shouldTerminateDaemon = false
    private var hidesNativeTitlebar: Bool { true }

    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.setActivationPolicy(.regular)
        NSApp.activate(ignoringOtherApps: true)
        installMinimalMenu()
        makeWindow()

        Task {
            do {
                try await daemon.ensureStarted()
                await MainActor.run {
                    self.shouldTerminateDaemon = true
                }
                await daemon.installHostIntegration()
                let profile = await daemon.queryProfile()
                await MainActor.run {
                    GhosttyTerminalView.configureDefaultProfile(profile)
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

    func applicationWillTerminate(_ notification: Notification) {
        terminateDaemonIfNeeded()
    }

    private func makeWindow() {
        rootController.view.frame = NSRect(x: 0, y: 0, width: 1100, height: 760)
        let hideTitleBar = hidesNativeTitlebar
        var styleMask: NSWindow.StyleMask = [
            .titled,
            .closable,
            .miniaturizable,
            .resizable,
        ]
        if hideTitleBar {
            styleMask.insert(.fullSizeContentView)
        }

        let window = EmacsHostWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1100, height: 760),
            styleMask: styleMask,
            backing: .buffered,
            defer: false
        )
        window.title = "Emacs Host"
        window.backgroundColor = .black
        window.isOpaque = true
        window.isReleasedWhenClosed = true
        window.acceptsMouseMovedEvents = true
        window.titlebarAppearsTransparent = hideTitleBar
        window.titleVisibility = hideTitleBar ? .hidden : .visible
        window.isMovableByWindowBackground = true
        window.contentViewController = rootController

        let titlebarLayout = WarpTitlebarLayout(
            window: window,
            chromeView: rootController.makeTitlebarChromeView()
        )
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

    private func terminateDaemonIfNeeded() {
        guard shouldTerminateDaemon else { return }

        let process = Process()
        process.executableURL = URL(fileURLWithPath: config.emacsclientPath)
        process.arguments = ["-s", config.serverName, "-e", "(kill-emacs)"]
        do {
            try process.run()
            process.waitUntilExit()
        } catch {
            return
        }
    }

    private func showStartupError(_ error: Error) {
        let alert = NSAlert()
        alert.alertStyle = .critical
        alert.messageText = "Emacs Host failed"
        alert.informativeText = String(describing: error)
        alert.runModal()
    }
}
