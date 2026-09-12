platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
    exposes [Stdout]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stdout_report": Host.report!,
    }
    targets: {}

import Host
import Stdout

main_for_host! : List(Str) => I32
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
        Err(_) => 1
    }
