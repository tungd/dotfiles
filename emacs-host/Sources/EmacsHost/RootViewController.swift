import AppKit
import SwiftUI

@MainActor
final class RootViewController: NSViewController {
    private struct Tab {
        let id: UUID
        let controller: EmacsTabViewController
    }

    private struct GridMetrics {
        let rows: Int
        let cols: Int
        let cellWidthPx: UInt32
        let cellHeightPx: UInt32
    }

    private final class ChromeHostingView<Content: View>: NSHostingView<Content> {
        override var mouseDownCanMoveWindow: Bool { true }

        override var safeAreaInsets: NSEdgeInsets {
            NSEdgeInsets(top: 0, left: 0, bottom: 0, right: 0)
        }
    }

    private final class RootContainerView: NSView {
        override var safeAreaInsets: NSEdgeInsets {
            NSEdgeInsets(top: 0, left: 0, bottom: 0, right: 0)
        }
    }

    private let config: AppConfig
    private let chromeModel = TabChromeModel()
    private let contentContainer = NSView()
    private var tabs: [Tab] = []
    private var selectedTabID: UUID?
    private var gridMetricsByTabID: [UUID: GridMetrics] = [:]
    private var tabCounter = 0
    private var isApplyingGridSize = false

    init(config: AppConfig) {
        self.config = config
        super.init(nibName: nil, bundle: nil)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func loadView() {
        let root = RootContainerView()
        root.wantsLayer = true
        root.layer?.backgroundColor = NSColor.black.cgColor

        contentContainer.translatesAutoresizingMaskIntoConstraints = false
        contentContainer.wantsLayer = true
        contentContainer.layer?.backgroundColor = NSColor.black.cgColor

        root.addSubview(contentContainer)

        NSLayoutConstraint.activate([
            contentContainer.leadingAnchor.constraint(equalTo: root.leadingAnchor),
            contentContainer.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            contentContainer.topAnchor.constraint(
                equalTo: root.topAnchor,
                constant: TabChromeView.height
            ),
            contentContainer.bottomAnchor.constraint(equalTo: root.bottomAnchor),
        ])

        view = root
    }

    func makeTitlebarChromeView() -> NSView {
        let chromeView = TabChromeView(
            model: chromeModel,
            onSelect: { [weak self] tabID in
                self?.select(tabID: tabID)
            },
            onNewTab: { [weak self] in
                self?.openNewTab(nil)
            }
        ).ignoresSafeArea()
        let chromeHost = ChromeHostingView(rootView: chromeView)
        chromeHost.translatesAutoresizingMaskIntoConstraints = false
        return chromeHost
    }

    @objc func openNewTab(_ sender: Any?) {
        tabCounter += 1
        do {
            let session = try TerminalSession(config: config, index: tabCounter)
            let controller = EmacsTabViewController(session: session)
            addChild(controller)
            controller.view.translatesAutoresizingMaskIntoConstraints = false
            contentContainer.addSubview(controller.view)

            NSLayoutConstraint.activate([
                controller.view.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor),
                controller.view.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor),
                controller.view.topAnchor.constraint(equalTo: contentContainer.topAnchor),
                controller.view.bottomAnchor.constraint(equalTo: contentContainer.bottomAnchor),
            ])

            tabs.append(Tab(id: session.id, controller: controller))
            chromeModel.appendTab(id: session.id, title: controller.title ?? session.title)
            select(tabID: session.id)
        } catch {
            present(error: error)
        }
    }

    func updateTitle(for controller: EmacsTabViewController, title: String) {
        guard let tab = tabs.first(where: { $0.controller === controller }) else {
            return
        }
        chromeModel.updateTitle(id: tab.id, title: title)
    }

    func updateGridMetrics(
        for controller: EmacsTabViewController,
        rows: Int,
        cols: Int,
        cellWidthPx: UInt32,
        cellHeightPx: UInt32
    ) {
        guard let tab = tabs.first(where: { $0.controller === controller }) else {
            return
        }

        let metrics = GridMetrics(
            rows: rows,
            cols: cols,
            cellWidthPx: cellWidthPx,
            cellHeightPx: cellHeightPx
        )
        gridMetricsByTabID[tab.id] = metrics

        if tab.id == selectedTabID {
            applyWindowGridSizing(metrics)
        }
    }

    func removeTab(for controller: EmacsTabViewController) {
        guard let index = tabs.firstIndex(where: { $0.controller === controller }) else {
            return
        }

        let removed = tabs.remove(at: index)
        let removedWasSelected = removed.id == selectedTabID

        removed.controller.session.delegate = nil
        removed.controller.view.removeFromSuperview()
        removed.controller.removeFromParent()
        chromeModel.removeTab(id: removed.id)
        gridMetricsByTabID.removeValue(forKey: removed.id)

        if tabs.isEmpty {
            selectedTabID = nil
            view.window?.close()
            return
        }

        if removedWasSelected {
            let nextIndex = min(index, tabs.count - 1)
            select(tabID: tabs[nextIndex].id)
        }
    }

    func performHostKeyEquivalent(with event: NSEvent) -> Bool {
        guard let selectedTabID,
              let tab = tabs.first(where: { $0.id == selectedTabID }) else {
            return false
        }

        return tab.controller.performHostKeyEquivalent(with: event)
    }

    private func select(tabID: UUID) {
        selectedTabID = tabID
        chromeModel.selectedTabID = tabID
        for tab in tabs {
            let selected = tab.id == tabID
            tab.controller.view.isHidden = !selected
            if selected {
                view.window?.makeFirstResponder(tab.controller.view)
                if let metrics = gridMetricsByTabID[tab.id] {
                    applyWindowGridSizing(metrics)
                }
            }
        }
    }

    private func applyWindowGridSizing(_ metrics: GridMetrics) {
        guard !isApplyingGridSize,
              metrics.rows > 0,
              metrics.cols > 0,
              metrics.cellWidthPx > 0,
              metrics.cellHeightPx > 0,
              let window = view.window,
              let contentView = window.contentView else {
            return
        }

        let scale = max(window.backingScaleFactor, 1)
        let cellWidth = CGFloat(metrics.cellWidthPx) / scale
        let cellHeight = CGFloat(metrics.cellHeightPx) / scale
        let targetSize = NSSize(
            width: CGFloat(metrics.cols) * cellWidth,
            height: TabChromeView.height + CGFloat(metrics.rows) * cellHeight
        )
        let currentSize = contentView.bounds.size

        window.contentResizeIncrements = NSSize(width: cellWidth, height: cellHeight)
        window.contentMinSize = NSSize(
            width: cellWidth * 40,
            height: TabChromeView.height + cellHeight * 10
        )

        guard abs(currentSize.width - targetSize.width) > 0.5
            || abs(currentSize.height - targetSize.height) > 0.5 else {
            return
        }

        isApplyingGridSize = true
        window.setContentSize(targetSize)
        isApplyingGridSize = false
    }

    private func present(error: Error) {
        let alert = NSAlert(error: error)
        alert.runModal()
    }
}
