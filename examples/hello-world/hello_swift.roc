app "hello_swift"
    packages { pf: "swift-platform" }
    imports []
    provides [ main ] to pf

main =
    host = "Swift"
    app = "Roc"

    "Hello \(host), meet \(app)"
