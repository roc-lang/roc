app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |args| {
    match args {
        [argv0, first, second] => Stdout.line!("${argv0}|${first}|${second}")
        _ => Stdout.line!("unexpected args: ${Str.inspect(args)}")
    }
    Ok({})
}
