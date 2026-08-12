platform "optional-field-host-boundary"
    requires {} { main! : List(Str) => Try({}, [Exit(I32)]) }
    exposes [Fallible]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted { "roc_fallible_line": Fallible.line! }
    targets: {}

main_for_host! : List(Str) => Try({}, [Exit(I32)])
main_for_host! = main!

import Fallible
