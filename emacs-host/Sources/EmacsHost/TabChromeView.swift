import SwiftUI

struct TabChromeItem: Identifiable, Equatable {
    let id: UUID
    var title: String
}

@MainActor
final class TabChromeModel: ObservableObject {
    @Published private(set) var tabs: [TabChromeItem] = []
    @Published var selectedTabID: UUID?

    func appendTab(id: UUID, title: String) {
        tabs.append(TabChromeItem(id: id, title: title))
    }

    func removeTab(id: UUID) {
        tabs.removeAll { $0.id == id }
        if selectedTabID == id {
            selectedTabID = nil
        }
    }

    func updateTitle(id: UUID, title: String) {
        guard let index = tabs.firstIndex(where: { $0.id == id }) else {
            return
        }
        tabs[index].title = title
    }
}

struct TabChromeView: View {
    @ObservedObject var model: TabChromeModel
    let onSelect: (UUID) -> Void
    let onNewTab: () -> Void

    static let height = WarpTitlebarMetrics.totalTitlebarHeight

    var body: some View {
        VStack(spacing: 0) {
            HStack(spacing: 0) {
                leadingSpacer

                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 0) {
                        ForEach(model.tabs) { tab in
                            WarpTabSegment(
                                title: tab.title,
                                isSelected: tab.id == model.selectedTabID,
                                action: {
                                    onSelect(tab.id)
                                }
                            )
                        }
                    }
                }
                .scrollClipDisabled()

                chromeIconButton("plus", action: onNewTab)
                    .frame(width: 36)
            }
            .frame(height: WarpTitlebarMetrics.tabBarHeight)

            Rectangle()
                .fill(Self.separator)
                .frame(height: WarpTitlebarMetrics.tabBarBorderHeight)
        }
        .frame(height: Self.height)
        .background(Self.chromeBackground)
    }

    private var leadingSpacer: some View {
        Color.clear
            .frame(width: 76, height: WarpTitlebarMetrics.tabBarHeight)
    }

    private func chromeIconButton(_ symbolName: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: symbolName)
                .font(.system(size: 14, weight: .medium))
                .symbolRenderingMode(.monochrome)
                .frame(width: 22, height: 22)
        }
        .buttonStyle(.plain)
        .foregroundStyle(Self.iconColor)
        .contentShape(Rectangle())
    }

    private static let chromeBackground = Color(red: 0.035, green: 0.035, blue: 0.035)
    private static let separator = Color(red: 0.14, green: 0.14, blue: 0.14)
    private static let iconColor = Color(red: 0.62, green: 0.62, blue: 0.62)
}

private struct WarpTabSegment: View {
    let title: String
    let isSelected: Bool
    let action: () -> Void

    @State private var isHovering = false

    var body: some View {
        Button(action: action) {
            Text(title)
                .font(.system(size: 12, weight: isSelected ? .semibold : .medium))
                .lineLimit(1)
                .truncationMode(.tail)
                .frame(maxWidth: .infinity)
                .padding(.horizontal, 16)
                .frame(width: 178, height: WarpTitlebarMetrics.tabBarHeight)
                .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .foregroundStyle(isSelected ? Self.activeText : Self.inactiveText)
        .background(backgroundColor)
        .overlay(alignment: .leading) {
            Rectangle()
                .fill(Self.separator)
                .frame(width: 1)
        }
        .overlay(alignment: .trailing) {
            Rectangle()
                .fill(Self.separator)
                .frame(width: 1)
        }
        .contentShape(Rectangle())
        .onHover { hovering in
            isHovering = hovering
        }
    }

    private var backgroundColor: Color {
        if isSelected {
            return Self.activeBackground
        }
        if isHovering {
            return Self.hoverBackground
        }
        return Self.inactiveBackground
    }

    private static let activeBackground = Color(red: 0.145, green: 0.145, blue: 0.145)
    private static let hoverBackground = Color(red: 0.09, green: 0.09, blue: 0.09)
    private static let inactiveBackground = Color(red: 0.045, green: 0.045, blue: 0.045)
    private static let separator = Color(red: 0.16, green: 0.16, blue: 0.16)
    private static let activeText = Color(red: 0.94, green: 0.94, blue: 0.94)
    private static let inactiveText = Color(red: 0.58, green: 0.58, blue: 0.58)
}
