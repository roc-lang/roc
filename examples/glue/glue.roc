app "rocLovesRust"
    packages { pf: "rust-platform/main.roc" }
    imports []
    provides [main] to pf


main : {} -> [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done] as Op
main =
    \{} -> 
        StdoutWrite "Roc <3 Rust!\n" \{} ->
            StderrWrite "Roc <3 Rust, also from stderr!\n" \{} ->
                Done
