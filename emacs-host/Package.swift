// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "emacs-host",
    platforms: [
        .macOS(.v15),
    ],
    products: [
        .executable(name: "emacs-host", targets: ["EmacsHost"]),
    ],
    dependencies: [],
    targets: [
        .target(name: "CEmacsHost"),
        .binaryTarget(name: "GhosttyKit", path: "GhosttyKit.xcframework"),
        .executableTarget(
            name: "EmacsHost",
            dependencies: [
                "CEmacsHost",
                "GhosttyKit",
            ],
            swiftSettings: [
                .swiftLanguageMode(.v5),
            ],
            linkerSettings: [
                .linkedFramework("AppKit"),
                .linkedFramework("Carbon"),
                .linkedFramework("CoreGraphics"),
                .linkedFramework("CoreText"),
                .linkedFramework("Metal"),
                .linkedFramework("QuartzCore"),
                .linkedLibrary("c++"),
            ]
        ),
    ]
)
