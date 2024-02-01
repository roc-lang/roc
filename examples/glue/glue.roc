app "rocLovesRust"
    packages { pf: "rust-platform/main.roc" }
    provides [main] to pf

main =
    msg = "Roc <3 Rust, also on stderr!\n"
    StdoutWrite "Roc <3 Rust!\n" \{} ->
        StderrWrite msg \{} ->
            Done
