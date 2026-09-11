platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted { "roc_stdout_line": Host.line! }
    targets: {}

import Host

main_for_host! : List(Str) => Try({}, [StdoutErr(Str)])
main_for_host! = |_args|
    match Host.line!("hello") {
        Ok({}) => Ok({})
        Err(StdoutErr(message)) => Err(StdoutErr(message))
    }
