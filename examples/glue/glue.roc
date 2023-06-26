app "rocLovesRust"
    packages { pf: "rust-platform/main.roc" }
    imports [pf.Task.{ Task, stdoutLine }]
    provides [main] to pf

#$ main : Task {} []
#$ main =
#$     msg = "Roc <3 Rust, also on stderr!\n"
#$     stdoutLine msg

main = 42u64
