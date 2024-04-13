app "ingested-file"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }
    imports [
        pf.Stdout,
        "ingested-file.roc" as ownCode : Str,
    ]
    provides [main] to pf

main =
    Stdout.line "\nThis roc file can print its own source code. The source is:\n\n$(ownCode)"
