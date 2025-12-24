app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Note: This platform uses an open union error type [Exit(I32), ..others]
# which means you can add your own error types in addition to Exit
main! : List(Str) => Try({}, [Exit(I32), ..others])
main! = |_args| {
    Stdout.line!("Hello from fx-open platform!")
    Ok({})
}
