app "ingested-file"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    provides [main] to pf

import pf.Stdout
import "ingested-file.roc" as ownCode : Str

main =
    Stdout.line "\nThis roc file can print its own source code. The source is:\n\n$(ownCode)"
