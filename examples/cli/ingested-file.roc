app "ingested-file"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    provides [main] to pf

import pf.Stdout
import "ingested-file.roc" as ownCode : Str

main =
    Stdout.line "\nThis roc file can print it's own source code. The source is:\n\n\(ownCode)"
