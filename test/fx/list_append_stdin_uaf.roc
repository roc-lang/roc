app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

str : Str -> Str
str = |s| s

main! = || {
    # This triggers a use-after-free bug
    # lines = [Stdin.line!()] works fine
    # but [].append(Stdin.line!()) panics when accessing elements
    lines = [].append(Stdin.line!())

    # Use for_each to output the line
    List.for_each!(lines, |line| Stdout.line!(str(line)))
}
