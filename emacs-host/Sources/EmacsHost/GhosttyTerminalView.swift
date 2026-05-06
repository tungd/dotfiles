import AppKit
import Carbon.HIToolbox
import GhosttyKit
import Metal
import QuartzCore

@MainActor
protocol GhosttyTerminalViewHost: AnyObject {
    func ghosttyTerminalView(_ view: GhosttyTerminalView, didSend bytes: [UInt8])
    func ghosttyTerminalView(_ view: GhosttyTerminalView, didResize rows: Int, cols: Int)
    func ghosttyTerminalView(_ view: GhosttyTerminalView, didSetTitle title: String)
}

private final class GhosttySurfaceContext {
    weak var view: GhosttyTerminalView?

    init(view: GhosttyTerminalView) {
        self.view = view
    }
}

private final class GhosttyRuntime {
    static let shared = GhosttyRuntime()

    private(set) var app: ghostty_app_t?
    private var config: ghostty_config_t?
    private var tickScheduled = false
    private let tickLock = NSLock()

    private init() {
        initialize()
    }

    deinit {
        if let app {
            ghostty_app_free(app)
        }
        if let config {
            ghostty_config_free(config)
        }
    }

    private func initialize() {
        let initResult = ghostty_init(UInt(CommandLine.argc), CommandLine.unsafeArgv)
        guard initResult == GHOSTTY_SUCCESS else {
            fputs("ghostty_init failed: \(initResult)\n", stderr)
            return
        }

        guard let primaryConfig = makeConfig(loadUserDefaults: true) else {
            return
        }

        var runtimeConfig = ghostty_runtime_config_s()
        runtimeConfig.userdata = Unmanaged.passUnretained(self).toOpaque()
        runtimeConfig.supports_selection_clipboard = true
        runtimeConfig.wakeup_cb = { userdata in
            guard let userdata else { return }
            Unmanaged<GhosttyRuntime>.fromOpaque(userdata)
                .takeUnretainedValue()
                .scheduleTick()
        }
        runtimeConfig.action_cb = { _, target, action in
            GhosttyRuntime.handleAction(target: target, action: action)
        }
        runtimeConfig.read_clipboard_cb = { userdata, _, state in
            GhosttyRuntime.readClipboard(userdata: userdata, state: state)
        }
        runtimeConfig.confirm_read_clipboard_cb = { userdata, text, state, _ in
            GhosttyRuntime.confirmClipboardRead(userdata: userdata, text: text, state: state)
        }
        runtimeConfig.write_clipboard_cb = { _, _, contents, count, _ in
            GhosttyRuntime.writeClipboard(contents: contents, count: count)
        }
        runtimeConfig.close_surface_cb = { userdata, _ in
            GhosttyRuntime.closeSurface(userdata: userdata)
        }

        if let app = ghostty_app_new(&runtimeConfig, primaryConfig) {
            self.app = app
            self.config = primaryConfig
            return
        }

        ghostty_config_free(primaryConfig)

        guard let fallbackConfig = makeConfig(loadUserDefaults: false),
              let fallbackApp = ghostty_app_new(&runtimeConfig, fallbackConfig) else {
            fputs("ghostty_app_new failed\n", stderr)
            return
        }

        self.app = fallbackApp
        self.config = fallbackConfig
    }

    private func makeConfig(loadUserDefaults: Bool) -> ghostty_config_t? {
        guard let config = ghostty_config_new() else {
            fputs("ghostty_config_new failed\n", stderr)
            return nil
        }

        if loadUserDefaults {
            ghostty_config_load_default_files(config)
        }

        let overrides = """
        macos-background-from-layer = true
        shell-integration = none
        window-padding-x = 0
        window-padding-y = 0
        confirm-close-surface = false
        """

        overrides.withCString { ptr in
            ghostty_config_load_string(
                config,
                ptr,
                UInt(overrides.lengthOfBytes(using: .utf8)),
                "emacs-host-overrides"
            )
        }
        ghostty_config_finalize(config)
        return config
    }

    private func scheduleTick() {
        tickLock.lock()
        if tickScheduled {
            tickLock.unlock()
            return
        }
        tickScheduled = true
        tickLock.unlock()

        DispatchQueue.main.async { [weak self] in
            guard let self else { return }
            self.tickLock.lock()
            self.tickScheduled = false
            self.tickLock.unlock()
            if let app = self.app {
                ghostty_app_tick(app)
            }
        }
    }

    private static func context(from userdata: UnsafeMutableRawPointer?) -> GhosttySurfaceContext? {
        guard let userdata else { return nil }
        return Unmanaged<GhosttySurfaceContext>.fromOpaque(userdata).takeUnretainedValue()
    }

    private static func surfaceContext(from surface: ghostty_surface_t?) -> GhosttySurfaceContext? {
        guard let surface else { return nil }
        return context(from: ghostty_surface_userdata(surface))
    }

    private static func handleAction(target: ghostty_target_s, action: ghostty_action_s) -> Bool {
        switch action.tag {
        case GHOSTTY_ACTION_RING_BELL:
            DispatchQueue.main.async {
                NSSound.beep()
            }
            return true
        case GHOSTTY_ACTION_SET_TITLE:
            let title = action.action.set_title.title.map(String.init(cString:)) ?? ""
            if target.tag == GHOSTTY_TARGET_SURFACE,
               let view = surfaceContext(from: target.target.surface)?.view {
                DispatchQueue.main.async {
                    view.host?.ghosttyTerminalView(view, didSetTitle: title)
                }
            }
            return true
        case GHOSTTY_ACTION_SET_TAB_TITLE:
            let title = action.action.set_tab_title.title.map(String.init(cString:)) ?? ""
            if target.tag == GHOSTTY_TARGET_SURFACE,
               let view = surfaceContext(from: target.target.surface)?.view {
                DispatchQueue.main.async {
                    view.host?.ghosttyTerminalView(view, didSetTitle: title)
                }
            }
            return true
        case GHOSTTY_ACTION_OPEN_URL:
            let openURL = action.action.open_url
            guard let pointer = openURL.url,
                  let string = String(
                    data: Data(bytes: pointer, count: Int(openURL.len)),
                    encoding: .utf8
                  ),
                  let url = URL(string: string) else {
                return false
            }
            DispatchQueue.main.async {
                NSWorkspace.shared.open(url)
            }
            return true
        default:
            return false
        }
    }

    private static func readClipboard(userdata: UnsafeMutableRawPointer?, state: UnsafeMutableRawPointer?) -> Bool {
        guard let context = context(from: userdata), state != nil else {
            return false
        }

        DispatchQueue.main.async {
            guard let view = context.view,
                  let surface = view.surface,
                  let state,
                  let text = NSPasteboard.general.string(forType: .string) else {
                return
            }
            text.withCString { ptr in
                ghostty_surface_complete_clipboard_request(surface, ptr, state, false)
            }
        }
        return true
    }

    private static func confirmClipboardRead(
        userdata: UnsafeMutableRawPointer?,
        text: UnsafePointer<CChar>?,
        state: UnsafeMutableRawPointer?
    ) {
        guard let context = context(from: userdata),
              let text,
              let state else {
            return
        }

        DispatchQueue.main.async {
            guard let surface = context.view?.surface else { return }
            ghostty_surface_complete_clipboard_request(surface, text, state, true)
        }
    }

    private static func writeClipboard(
        contents: UnsafePointer<ghostty_clipboard_content_s>?,
        count: Int
    ) {
        guard let contents, count > 0 else { return }
        let buffer = UnsafeBufferPointer(start: contents, count: count)

        var fallback: String?
        for item in buffer {
            guard let data = item.data else { continue }
            let value = String(cString: data)
            if let mime = item.mime.map(String.init(cString:)),
               mime.hasPrefix("text/plain") {
                DispatchQueue.main.async {
                    NSPasteboard.general.clearContents()
                    NSPasteboard.general.setString(value, forType: .string)
                }
                return
            }
            if fallback == nil {
                fallback = value
            }
        }

        if let fallback {
            DispatchQueue.main.async {
                NSPasteboard.general.clearContents()
                NSPasteboard.general.setString(fallback, forType: .string)
            }
        }
    }

    private static func closeSurface(userdata: UnsafeMutableRawPointer?) {
        guard let context = context(from: userdata) else { return }
        DispatchQueue.main.async {
            context.view?.append(Array("\r\n[ghostty surface closed]\r\n".utf8))
        }
    }
}

final class GhosttyTerminalView: NSView {
    weak var host: GhosttyTerminalViewHost?

    fileprivate private(set) var surface: ghostty_surface_t?

    private var context: Unmanaged<GhosttySurfaceContext>?
    private var pendingOutput: [[UInt8]] = []
    private var lastRows = 0
    private var lastCols = 0
    private var lastPixelWidth: UInt32 = 0
    private var lastPixelHeight: UInt32 = 0
    private var lastXScale: CGFloat = 0
    private var lastYScale: CGFloat = 0

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        configure()
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    deinit {
        if let surface {
            ghostty_surface_free(surface)
        }
        context?.release()
    }

    override var acceptsFirstResponder: Bool { true }
    override var isOpaque: Bool { true }

    override func makeBackingLayer() -> CALayer {
        let layer = CAMetalLayer()
        layer.pixelFormat = .bgra8Unorm
        layer.framebufferOnly = false
        layer.isOpaque = true
        return layer
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        _ = ensureSurface()
        updateDisplayID()
        updateSurfaceGeometry()
        if window?.firstResponder === self {
            setGhosttyFocus(true)
        }
    }

    override func viewDidChangeBackingProperties() {
        super.viewDidChangeBackingProperties()
        updateSurfaceGeometry()
    }

    override func layout() {
        super.layout()
        updateSurfaceGeometry()
    }

    override func becomeFirstResponder() -> Bool {
        setGhosttyFocus(true)
        return true
    }

    override func resignFirstResponder() -> Bool {
        setGhosttyFocus(false)
        return true
    }

    func append(_ bytes: [UInt8]) {
        guard let surface = ensureSurface() else {
            pendingOutput.append(bytes)
            return
        }

        bytes.withUnsafeBufferPointer { buffer in
            guard let baseAddress = buffer.baseAddress else { return }
            ghostty_surface_process_output(surface, baseAddress, UInt(bytes.count))
        }
        ghostty_surface_refresh(surface)
        refreshSessionSizeFromSurface()
    }

    private func configure() {
        wantsLayer = true
        layer?.backgroundColor = NSColor.black.cgColor
    }

    private func ensureSurface() -> ghostty_surface_t? {
        if let surface {
            return surface
        }
        guard let app = GhosttyRuntime.shared.app else {
            return nil
        }

        let context = Unmanaged.passRetained(GhosttySurfaceContext(view: self))
        self.context = context

        var config = ghostty_surface_config_new()
        config.platform_tag = GHOSTTY_PLATFORM_MACOS
        config.platform = ghostty_platform_u(macos: ghostty_platform_macos_s(
            nsview: Unmanaged.passUnretained(self).toOpaque()
        ))
        config.userdata = context.toOpaque()
        config.scale_factor = Double(currentBackingScale())
        config.context = GHOSTTY_SURFACE_CONTEXT_TAB
        config.io_mode = GHOSTTY_SURFACE_IO_MANUAL
        config.io_write_userdata = context.toOpaque()
        config.io_write_cb = { userdata, pointer, length in
            guard let userdata, length > 0 else { return }
            let context = Unmanaged<GhosttySurfaceContext>
                .fromOpaque(userdata)
                .takeUnretainedValue()
            let bytes = Array(UnsafeBufferPointer(start: pointer, count: Int(length))).map {
                UInt8(bitPattern: $0)
            }
            DispatchQueue.main.async {
                guard let view = context.view else { return }
                view.host?.ghosttyTerminalView(view, didSend: bytes)
            }
        }

        guard let createdSurface = ghostty_surface_new(app, &config) else {
            context.release()
            self.context = nil
            return nil
        }

        surface = createdSurface
        updateDisplayID()
        updateSurfaceGeometry()
        setGhosttyFocus(window?.firstResponder === self)

        if !pendingOutput.isEmpty {
            let output = pendingOutput
            pendingOutput.removeAll()
            for bytes in output {
                append(bytes)
            }
        }

        return createdSurface
    }

    private func currentBackingScale() -> CGFloat {
        max(
            1.0,
            window?.backingScaleFactor
                ?? layer?.contentsScale
                ?? NSScreen.main?.backingScaleFactor
                ?? 1.0
        )
    }

    private func updateDisplayID() {
        guard let surface,
              let displayID = (window?.screen ?? NSScreen.main)?.deviceDescription[
                NSDeviceDescriptionKey("NSScreenNumber")
              ] as? UInt32,
              displayID != 0 else {
            return
        }
        ghostty_surface_set_display_id(surface, displayID)
    }

    private func updateSurfaceGeometry() {
        guard let surface else { return }
        guard bounds.width > 0, bounds.height > 0 else { return }

        let backingSize = convertToBacking(NSRect(origin: .zero, size: bounds.size)).size
        let width = pixelDimension(backingSize.width)
        let height = pixelDimension(backingSize.height)
        guard width > 0, height > 0 else { return }

        let xScale = backingSize.width / bounds.width
        let yScale = backingSize.height / bounds.height
        let scaleChanged = abs(xScale - lastXScale) > 0.0001 || abs(yScale - lastYScale) > 0.0001
        let sizeChanged = width != lastPixelWidth || height != lastPixelHeight

        CATransaction.begin()
        CATransaction.setDisableActions(true)
        layer?.contentsScale = currentBackingScale()
        if let metalLayer = layer as? CAMetalLayer {
            metalLayer.drawableSize = CGSize(width: CGFloat(width), height: CGFloat(height))
        }
        CATransaction.commit()

        if scaleChanged {
            ghostty_surface_set_content_scale(surface, xScale, yScale)
            lastXScale = xScale
            lastYScale = yScale
        }

        if sizeChanged {
            ghostty_surface_set_size(surface, width, height)
            lastPixelWidth = width
            lastPixelHeight = height
            refreshSessionSizeFromSurface()
        }
    }

    private func refreshSessionSizeFromSurface() {
        guard let surface else { return }
        let size = ghostty_surface_size(surface)
        let rows = Int(size.rows)
        let cols = Int(size.columns)
        guard rows > 0, cols > 0 else { return }
        guard rows != lastRows || cols != lastCols else { return }
        lastRows = rows
        lastCols = cols
        host?.ghosttyTerminalView(self, didResize: rows, cols: cols)
    }

    private func pixelDimension(_ value: CGFloat) -> UInt32 {
        UInt32(max(1, min(CGFloat(UInt32.max), floor(value))))
    }

    private func setGhosttyFocus(_ focused: Bool) {
        guard let surface = ensureSurface() else { return }
        ghostty_surface_set_focus(surface, focused)
        if focused {
            updateDisplayID()
        }
    }

    override func keyDown(with event: NSEvent) {
        guard let surface = ensureSurface() else {
            super.keyDown(with: event)
            return
        }

        var key = ghosttyKey(from: event, action: event.isARepeat ? GHOSTTY_ACTION_REPEAT : GHOSTTY_ACTION_PRESS)
        let text = textForKey(event)
        if let text, shouldSendText(text) {
            text.withCString { pointer in
                key.text = pointer
                _ = ghostty_surface_key(surface, key)
            }
        } else {
            key.text = nil
            _ = ghostty_surface_key(surface, key)
        }
        ghostty_surface_refresh(surface)
    }

    override func keyUp(with event: NSEvent) {
        guard let surface = ensureSurface() else { return }
        var key = ghosttyKey(from: event, action: GHOSTTY_ACTION_RELEASE)
        key.text = nil
        _ = ghostty_surface_key(surface, key)
    }

    override func flagsChanged(with event: NSEvent) {
        guard let surface = ensureSurface(),
              let action = modifierAction(for: event) else {
            super.flagsChanged(with: event)
            return
        }
        var key = ghosttyKey(from: event, action: action)
        key.text = nil
        key.unshifted_codepoint = 0
        _ = ghostty_surface_key(surface, key)
    }

    override func insertText(_ insertString: Any) {
        insertText(insertString, replacementRange: NSRange(location: NSNotFound, length: 0))
    }

    func insertText(_ insertString: Any, replacementRange: NSRange) {
        guard let surface = ensureSurface() else { return }
        let text: String
        if let attributed = insertString as? NSAttributedString {
            text = attributed.string
        } else if let string = insertString as? String {
            text = string
        } else {
            text = String(describing: insertString)
        }
        guard shouldSendText(text) else { return }
        text.withCString { pointer in
            ghostty_surface_text_input(surface, pointer, UInt(text.lengthOfBytes(using: .utf8)))
        }
    }

    override func mouseDown(with event: NSEvent) {
        sendMouseButton(event, state: GHOSTTY_MOUSE_PRESS, button: GHOSTTY_MOUSE_LEFT)
    }

    override func mouseUp(with event: NSEvent) {
        sendMouseButton(event, state: GHOSTTY_MOUSE_RELEASE, button: GHOSTTY_MOUSE_LEFT)
    }

    override func rightMouseDown(with event: NSEvent) {
        sendMouseButton(event, state: GHOSTTY_MOUSE_PRESS, button: GHOSTTY_MOUSE_RIGHT)
    }

    override func rightMouseUp(with event: NSEvent) {
        sendMouseButton(event, state: GHOSTTY_MOUSE_RELEASE, button: GHOSTTY_MOUSE_RIGHT)
    }

    override func otherMouseDown(with event: NSEvent) {
        sendMouseButton(event, state: GHOSTTY_MOUSE_PRESS, button: GHOSTTY_MOUSE_MIDDLE)
    }

    override func otherMouseUp(with event: NSEvent) {
        sendMouseButton(event, state: GHOSTTY_MOUSE_RELEASE, button: GHOSTTY_MOUSE_MIDDLE)
    }

    override func mouseMoved(with event: NSEvent) {
        sendMousePosition(event)
    }

    override func mouseDragged(with event: NSEvent) {
        sendMousePosition(event)
    }

    override func rightMouseDragged(with event: NSEvent) {
        sendMousePosition(event)
    }

    override func otherMouseDragged(with event: NSEvent) {
        sendMousePosition(event)
    }

    override func scrollWheel(with event: NSEvent) {
        guard let surface = ensureSurface() else { return }
        sendMousePosition(event)

        var x = event.scrollingDeltaX
        var y = event.scrollingDeltaY
        let precise = event.hasPreciseScrollingDeltas
        if precise {
            x *= 2
            y *= 2
        }

        var mods: Int32 = 0
        if precise {
            mods |= 0b0000_0001
        }
        mods |= mouseMomentum(from: event) << 1

        ghostty_surface_mouse_scroll(surface, x, y, ghostty_input_scroll_mods_t(mods))
    }

    private func sendMouseButton(
        _ event: NSEvent,
        state: ghostty_input_mouse_state_e,
        button: ghostty_input_mouse_button_e
    ) {
        guard let surface = ensureSurface() else { return }
        window?.makeFirstResponder(self)
        sendMousePosition(event)
        _ = ghostty_surface_mouse_button(surface, state, button, modsFromEvent(event))
    }

    private func sendMousePosition(_ event: NSEvent) {
        guard let surface else { return }
        let point = convert(event.locationInWindow, from: nil)
        ghostty_surface_mouse_pos(
            surface,
            point.x,
            bounds.height - point.y,
            modsFromEvent(event)
        )
    }

    private func mouseMomentum(from event: NSEvent) -> Int32 {
        switch event.momentumPhase {
        case .began:
            Int32(GHOSTTY_MOUSE_MOMENTUM_BEGAN.rawValue)
        case .stationary:
            Int32(GHOSTTY_MOUSE_MOMENTUM_STATIONARY.rawValue)
        case .changed:
            Int32(GHOSTTY_MOUSE_MOMENTUM_CHANGED.rawValue)
        case .ended:
            Int32(GHOSTTY_MOUSE_MOMENTUM_ENDED.rawValue)
        case .cancelled:
            Int32(GHOSTTY_MOUSE_MOMENTUM_CANCELLED.rawValue)
        case .mayBegin:
            Int32(GHOSTTY_MOUSE_MOMENTUM_MAY_BEGIN.rawValue)
        default:
            Int32(GHOSTTY_MOUSE_MOMENTUM_NONE.rawValue)
        }
    }

    private func ghosttyKey(from event: NSEvent, action: ghostty_input_action_e) -> ghostty_input_key_s {
        var key = ghostty_input_key_s()
        key.action = action
        key.keycode = UInt32(event.keyCode)
        key.mods = modsFromEvent(event)
        key.consumed_mods = consumedMods(from: event.modifierFlags)
        key.unshifted_codepoint = unshiftedCodepoint(from: event)
        key.composing = false
        return key
    }

    private func textForKey(_ event: NSEvent) -> String? {
        if event.modifierFlags.contains(.control) {
            return event.charactersIgnoringModifiers ?? event.characters
        }
        return event.characters
    }

    private func modsFromEvent(_ event: NSEvent) -> ghostty_input_mods_e {
        mods(from: event.modifierFlags)
    }

    private func mods(from flags: NSEvent.ModifierFlags) -> ghostty_input_mods_e {
        var raw = GHOSTTY_MODS_NONE.rawValue
        if flags.contains(.shift) { raw |= GHOSTTY_MODS_SHIFT.rawValue }
        if flags.contains(.control) { raw |= GHOSTTY_MODS_CTRL.rawValue }
        if flags.contains(.option) { raw |= GHOSTTY_MODS_ALT.rawValue }
        if flags.contains(.command) { raw |= GHOSTTY_MODS_SUPER.rawValue }
        if flags.contains(.capsLock) { raw |= GHOSTTY_MODS_CAPS.rawValue }
        return ghostty_input_mods_e(rawValue: raw)
    }

    private func consumedMods(from flags: NSEvent.ModifierFlags) -> ghostty_input_mods_e {
        var raw = GHOSTTY_MODS_NONE.rawValue
        if flags.contains(.shift) { raw |= GHOSTTY_MODS_SHIFT.rawValue }
        if flags.contains(.option) { raw |= GHOSTTY_MODS_ALT.rawValue }
        return ghostty_input_mods_e(rawValue: raw)
    }

    private func unshiftedCodepoint(from event: NSEvent) -> UInt32 {
        guard let scalar = (event.charactersIgnoringModifiers ?? event.characters)?
            .unicodeScalars
            .first else {
            return 0
        }
        return scalar.value
    }

    private func modifierAction(for event: NSEvent) -> ghostty_input_action_e? {
        let flags = event.modifierFlags
        let active: Bool
        switch Int(event.keyCode) {
        case kVK_CapsLock:
            active = flags.contains(.capsLock)
        case kVK_Shift, kVK_RightShift:
            active = flags.contains(.shift)
        case kVK_Control, kVK_RightControl:
            active = flags.contains(.control)
        case kVK_Option, kVK_RightOption:
            active = flags.contains(.option)
        case kVK_Command, kVK_RightCommand:
            active = flags.contains(.command)
        default:
            return nil
        }
        return active ? GHOSTTY_ACTION_PRESS : GHOSTTY_ACTION_RELEASE
    }

    private func shouldSendText(_ text: String) -> Bool {
        guard !text.isEmpty else { return false }
        if text.count == 1, let scalar = text.unicodeScalars.first {
            return scalar.value >= 0x20 && scalar.value != 0x7F
        }
        return true
    }
}
