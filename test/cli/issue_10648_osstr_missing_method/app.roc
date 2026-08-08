app [main!] { pf: platform "platform/main.roc" }

import pf.Stdout

main! = |args| {
    # repro for https://github.com/roc-lang/roc/issues/10648
    # OsStr exposes display/to_str_try, so missing to_str should be a diagnostic.
    arg = List.get(args, 1) ? |_| Exit(1)
    Stdout.line!(arg.to_str()) ? |_| Exit(2)

    Ok({})
}
