platform ""
    requires {} { main! : { name : Str } => Try(_, [Exit(I8), ..]) }
    exposes [Echo]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted { "roc_echo_line": Echo.line! }

import Echo

main_for_host! : {} => I8
main_for_host! = |_|
    match main!({ name: "Roc" }) {
        Ok(_) => 0
        Err(Exit(code)) => code
        Err(_) => 1
    }
