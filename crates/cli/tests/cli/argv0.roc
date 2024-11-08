app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
}

import pf.Stdout
import pf.Arg

main =
    args = Arg.list! {}
    when List.first args is
        Ok argv0 -> Stdout.line argv0
        Err ListWasEmpty -> Stdout.line "Failed: argv was empty"
