app [main!] { pf: platform "./test/fx/platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!(Str.inspect(List.sublist([[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]], { start: 2, len: 9 })))
}
