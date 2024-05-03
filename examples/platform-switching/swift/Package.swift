// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "swift-host",
    platforms: [
       .macOS(.v13),
    ],
    products: [
        .library(
            name: "host", 
            type: .static,
            targets: ["Host","Roc"]
        ),
    ],
    targets: [
        .target(name: "Host"),
        .target(name: "Roc", dependencies: [])
    ]
)