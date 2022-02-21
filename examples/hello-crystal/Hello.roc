app "hello-crystal"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main =
    host = "Crystal"
    app = "Roc"

    "Hello \(host), meet \(app)"
