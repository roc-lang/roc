app "hello-swift"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

main =
    host = "Swift"
    app = "Roc"

    "Hello \(host), meet \(app)"
