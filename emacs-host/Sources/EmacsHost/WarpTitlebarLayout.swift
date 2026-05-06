import AppKit

@MainActor
final class WarpTitlebarLayout {
    private weak var window: NSWindow?
    private var constraints: [NSLayoutConstraint] = []

    init(window: NSWindow) {
        self.window = window
    }

    func apply() {
        guard
            let window,
            let closeButton = window.standardWindowButton(.closeButton),
            let titlebarView = closeButton.superview,
            let titlebarContainerView = titlebarView.superview
        else {
            return
        }

        configureTitlebarFrame(
            window: window,
            titlebarContainerView: titlebarContainerView,
            height: WarpTitlebarMetrics.totalTitlebarHeight
        )

        NSLayoutConstraint.deactivate(constraints)
        constraints.removeAll()

        titlebarView.translatesAutoresizingMaskIntoConstraints = false
        constraints.append(contentsOf: [
            titlebarView.heightAnchor.constraint(
                equalToConstant: WarpTitlebarMetrics.totalTitlebarHeight
            ),
            titlebarView.topAnchor.constraint(equalTo: titlebarContainerView.topAnchor),
            titlebarView.leadingAnchor.constraint(equalTo: titlebarContainerView.leadingAnchor),
            titlebarView.trailingAnchor.constraint(equalTo: titlebarContainerView.trailingAnchor),
        ])

        configureTrafficLights(in: titlebarView, window: window)
        NSLayoutConstraint.activate(constraints)
    }

    private func configureTitlebarFrame(
        window: NSWindow,
        titlebarContainerView: NSView,
        height: CGFloat
    ) {
        var frame = titlebarContainerView.frame
        frame.size.height = height
        frame.origin.y = window.frame.height - height
        titlebarContainerView.frame = frame
    }

    private func configureTrafficLights(in titlebarView: NSView, window: NSWindow) {
        let buttonTypes: [NSWindow.ButtonType] = [
            .closeButton,
            .miniaturizeButton,
            .zoomButton,
        ]

        for (index, buttonType) in buttonTypes.enumerated() {
            guard let button = window.standardWindowButton(buttonType) else {
                continue
            }

            button.translatesAutoresizingMaskIntoConstraints = false

            let xOffset = WarpTitlebarMetrics.trafficLightLeftMargin
                + CGFloat(index)
                    * (WarpTitlebarMetrics.trafficLightSize + WarpTitlebarMetrics.trafficLightSpacing)

            constraints.append(contentsOf: [
                button.widthAnchor.constraint(equalToConstant: WarpTitlebarMetrics.trafficLightSize),
                button.heightAnchor.constraint(equalToConstant: WarpTitlebarMetrics.trafficLightSize),
                button.leadingAnchor.constraint(equalTo: titlebarView.leadingAnchor, constant: xOffset),
                button.centerYAnchor.constraint(
                    equalTo: titlebarView.centerYAnchor,
                    constant: WarpTitlebarMetrics.trafficLightCenterYOffset
                ),
            ])
        }
    }
}
