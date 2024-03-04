app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

import pf.Stdout
import "ingested-file.roc" as ownCode : Str

main =
    Stdout.line! "\nThis roc file can print its own source code. The source is:\n\n$(ownCode)"
