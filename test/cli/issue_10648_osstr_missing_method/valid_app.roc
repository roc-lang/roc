app [main!] { pf: platform "platform/main.roc" }

import pf.Stdout

main! = |args| {
    arg = List.get(args, 1) ? |_| Exit(1)
    Stdout.line!(arg.display()) ? |_| Exit(2)

    Ok({})
}
