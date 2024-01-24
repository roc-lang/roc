app "ingested-file"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [
        pf.Stdout,
        "ingested-file.roc" as ownCode : Str,
    ]
    provides [main] to pf

main =
    Stdout.line "\nThis roc file can print it's own source code. The source is:\n\n\(ownCode)"
