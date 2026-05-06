import AppKit

final class EmacsTabViewController: NSViewController, TerminalSessionDelegate, GhosttyTerminalViewHost {
    let session: TerminalSession
    private let terminalView = GhosttyTerminalView(frame: .zero)

    init(session: TerminalSession) {
        self.session = session
        super.init(nibName: nil, bundle: nil)
        session.delegate = self
        title = session.title
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func loadView() {
        terminalView.host = self
        view = terminalView
    }

    override func viewDidAppear() {
        super.viewDidAppear()
        view.window?.makeFirstResponder(terminalView)
    }

    func terminalSession(_ session: TerminalSession, didReceive bytes: [UInt8]) {
        terminalView.append(bytes)
    }

    func terminalSession(_ session: TerminalSession, didReceive event: HostEvent) {
        switch event.command {
        case "tab-title":
            title = event.payload.isEmpty ? session.title : event.payload
            (parent as? RootViewController)?.updateTitle(for: self, title: title ?? session.title)
        default:
            break
        }
    }

    func terminalSessionDidExit(_ session: TerminalSession) {
        (parent as? RootViewController)?.removeTab(for: self)
    }

    func ghosttyTerminalView(_ view: GhosttyTerminalView, didSend bytes: [UInt8]) {
        session.send(bytes)
    }

    func ghosttyTerminalView(_ view: GhosttyTerminalView, didResize rows: Int, cols: Int) {
        session.resize(rows: rows, cols: cols)
    }

    func ghosttyTerminalView(_ view: GhosttyTerminalView, didSetTitle title: String) {
        self.title = title.isEmpty ? session.title : title
        (parent as? RootViewController)?.updateTitle(for: self, title: self.title ?? session.title)
    }

    deinit {
        session.close()
    }
}
