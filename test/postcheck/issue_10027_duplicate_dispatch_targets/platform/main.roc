platform ""
    requires {} { main! : () => {} }
    exposes [OsStr, Path]
    packages {
        path: "../pkg/main.roc",
    }
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {}

import OsStr
import Path

main_for_host! : List([Utf8(Str)]) => I32
main_for_host! = |_args| {
    main!()
    0
}
