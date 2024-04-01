app "rocLovesRust"
    packages { pf: "rust-platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

main = Task.stdoutLine "Roc <3 Rust!\n"
