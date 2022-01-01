app "hello-swift"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main =
    host = "Swift"
    app = "Roc"

    "Hello \(host), meet \(app)"
