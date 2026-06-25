platform ""
    requires {} { main! : List(Str) => Try(_, [Exit(I8), ..]) }
    exposes [Echo]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted { "roc_echo_line": Echo.line! }

import Echo

main_for_host! : List(Str) => I8
main_for_host! = |args|
    match main!(args) {
        Ok(_) => 0
        Err(Exit(code)) => code
        Err(other) => {
            Echo.line!("Program exited with error: ${Str.inspect(other)}")
            1
        }
    }
