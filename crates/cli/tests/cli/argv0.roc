app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
}

import pf.Stdout
import pf.Arg

main =
    args = Arg.list! {}
    when List.first args is
        Ok argv0 -> Stdout.line argv0
        Err ListWasEmpty -> Stdout.line "Failed: argv was empty"
