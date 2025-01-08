app [main] { pf: platform "rust-platform/main.roc" }

main =
    msg = "Roc <3 Rust, also on stderr!\n"
    StdoutWrite("Roc <3 Rust!\n", \{} ->
        StderrWrite(msg, \{} ->
            Done))
