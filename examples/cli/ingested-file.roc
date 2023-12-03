app "ingested-file"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "ingested-file.roc" as ownCode : Str,
    ]
    provides [main] to pf

main =
    Stdout.line "\nThis roc file can print it's own source code. The source is:\n\n\(ownCode)"
