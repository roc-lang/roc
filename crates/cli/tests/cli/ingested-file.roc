app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br" }

import pf.Stdout
import "ingested-file.roc" as ownCode : Str

main =
    Stdout.line! "\nThis roc file can print its own source code. The source is:\n\n$(ownCode)"
