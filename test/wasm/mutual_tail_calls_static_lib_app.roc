app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Each recursive call is a tail call to the other function, so the chain runs
# in constant stack space on WebAssembly too.
count_down! : U64, Str => Str
count_down! = |n, label| if n == 0 label else bounce!(n - 1, label)

bounce! : U64, Str => Str
bounce! = |n, label| {
    if n == 500_000 {
        Stdout.line!("halfway")
    }
    if n == 0 label else count_down!(n - 1, Str.concat(label, ""))
}

main! = || {
    count_down!(1_000_001, "a result string long enough to live on the heap")
}
