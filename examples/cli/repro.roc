app "repro"
    packages { pf: "effects-platform/main.roc" }
    imports [pf.Effect]
    provides [main] to pf

main =
    base64 = "blah"

    Effect.putLine "base64: \(base64)"
