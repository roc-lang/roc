platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
    exposes [OsStr, Env, Stdout]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_env_var": Host.var!,
        "roc_stdout_line": Host.line!,
    }
    targets: {}

import OsStr
import Host
import Env
import Stdout

main_for_host! : List(Str) => I32
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
        Err(other) => {
            _ = Host.line!(Str.inspect(other))
            1
        }
    }
