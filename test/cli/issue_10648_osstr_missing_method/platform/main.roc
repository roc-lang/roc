platform ""
    requires {} { main! : List(OsStr) => Try({}, [Exit(I32), ..]) }
    exposes [OsStr, Stdout]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stdout_line": Stdout.line!,
    }
    targets: {}

import OsStr exposing [OsStr]
import Stdout

main_for_host! : List(OsStr) => I32
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
        Err(_) => 1
    }
