import AppKit
import SwiftUI

@MainActor
final class RootViewController: NSViewController {
    private struct Tab {
        let id: UUID
        let controller: EmacsTabViewController
    }

    private final class ChromeHostingView<Content: View>: NSHostingView<Content> {
        override var mouseDownCanMoveWindow: Bool { true }

        override var safeAreaInsets: NSEdgeInsets {
            NSEdgeInsets(top: 0, left: 0, bottom: 0, right: 0)
        }
    }

    private let config: AppConfig
    private let chromeModel = TabChromeModel()
    private let contentContainer = NSView()
    private var tabs: [Tab] = []
    private var selectedTabID: UUID?
    private var tabCounter = 0

    init(config: AppConfig) {
        self.config = config
        super.init(nibName: nil, bundle: nil)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func loadView() {
        let root = NSView()
        root.wantsLayer = true
        root.layer?.backgroundColor = NSColor.black.cgColor

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

        contentContainer.translatesAutoresizingMaskIntoConstraints = false
        contentContainer.wantsLayer = true
        contentContainer.layer?.backgroundColor = NSColor.black.cgColor

        root.addSubview(chromeHost)
        root.addSubview(contentContainer)

        NSLayoutConstraint.activate([
            chromeHost.leadingAnchor.constraint(equalTo: root.leadingAnchor),
            chromeHost.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            chromeHost.topAnchor.constraint(equalTo: root.topAnchor),
            chromeHost.heightAnchor.constraint(equalToConstant: TabChromeView.height),

            contentContainer.leadingAnchor.constraint(equalTo: root.leadingAnchor),
            contentContainer.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            contentContainer.topAnchor.constraint(equalTo: chromeHost.bottomAnchor),
            contentContainer.bottomAnchor.constraint(equalTo: root.bottomAnchor),
        ])

        view = root
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

    private func select(tabID: UUID) {
        selectedTabID = tabID
        chromeModel.selectedTabID = tabID
        for tab in tabs {
            let selected = tab.id == tabID
            tab.controller.view.isHidden = !selected
            if selected {
                view.window?.makeFirstResponder(tab.controller.view)
            }
        }
    }

    private func present(error: Error) {
        let alert = NSAlert(error: error)
        alert.runModal()
    }
}
