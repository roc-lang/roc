# A platform whose own compile-time constant, `platform_len`, uses
# `Echo.traced` at `List(U64)`: the platform's finalization program evaluates
# that specialization, and an app using `Echo.traced` at the same type makes
# the pairing program evaluate it again.
platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    exposes [Echo]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted { "roc_echo_line": Echo.line! }

import Echo

platform_len : U64
platform_len = List.len(List.append(Echo.traced, 1.U64))

main_for_host! : List(Str) => I8
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
        Err(other) => {
            Echo.line!("Program exited with error: ${Str.inspect(other)}")
            1
        }
    }
