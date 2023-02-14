app "rocLovesRust"
    packages { pf: "rust-platform/main.roc" }
    imports []
    provides [main] to pf

main =
    StdoutWrite "Roc <3 Rust!\n" \{} ->
        StdoutWrite "Roc <3 Rust!\n" \{} ->
            Done
