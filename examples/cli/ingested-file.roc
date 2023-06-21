app "ingested-file"
    packages { pf: "cli-platform/main.roc" }
    imports [
        pf.Stdout,
        "ingested-file.roc" as ownCode : Str,
    ]
    provides [main] to pf

main =
    Stdout.line "\nThis roc file can print it's own source code. The source is:\n\n\(ownCode)"
