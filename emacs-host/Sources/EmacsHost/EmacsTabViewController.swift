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
        let session = self.session
        terminalView.inputHandler = { [weak session] bytes in
            session?.send(bytes)
        }
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
        case "clipboard-write":
            writeClipboard(event.payload)
        case "clipboard-read":
            sendClipboardContents(target: event.payload)
        default:
            break
        }
    }

    func terminalSessionDidExit(_ session: TerminalSession) {
        (parent as? RootViewController)?.removeTab(for: self)
    }

    func performHostKeyEquivalent(with event: NSEvent) -> Bool {
        terminalView.performHostKeyEquivalent(with: event)
    }

    func ghosttyTerminalView(_ view: GhosttyTerminalView, didSend bytes: [UInt8]) {
        session.send(bytes)
    }

    func ghosttyTerminalView(
        _ view: GhosttyTerminalView,
        didUpdateGrid rows: Int,
        cols: Int,
        cellWidthPx: UInt32,
        cellHeightPx: UInt32
    ) {
        session.resize(rows: rows, cols: cols)
        (parent as? RootViewController)?.updateGridMetrics(
            for: self,
            rows: rows,
            cols: cols,
            cellWidthPx: cellWidthPx,
            cellHeightPx: cellHeightPx
        )
    }

    func ghosttyTerminalView(_ view: GhosttyTerminalView, didSetTitle title: String) {
        self.title = title.isEmpty ? session.title : title
        (parent as? RootViewController)?.updateTitle(for: self, title: self.title ?? session.title)
    }

    private func writeClipboard(_ text: String) {
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(text, forType: .string)
    }

    private func sendClipboardContents(target: String) {
        let text = NSPasteboard.general.string(forType: .string) ?? ""
        let encoded = Data(text.utf8).base64EncodedString()
        let replyTarget = target.isEmpty ? "c" : target
        session.send(Array("\u{1B}]52;\(replyTarget);\(encoded)\u{7}".utf8))
    }

    deinit {
        session.close()
    }
}
