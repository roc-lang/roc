platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    exposes [Echo]
    packages {}
    provides { main_for_host!: "main" }

import Echo

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
