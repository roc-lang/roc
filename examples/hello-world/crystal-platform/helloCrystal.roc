app "helloCrystal"
    packages { pf: "." }
    imports []
    provides [ main ] to pf

main =
    host = "Crystal"
    app = "Roc"

    "Hello \(host), meet \(app)"
