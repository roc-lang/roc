app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

rev : List(a) -> List(a)
rev = |l| {
    var $acc = []
    for e in l {
        $acc = List.concat([e], $acc)
    }
    $acc
}

expect rev(["a", "b", "c"]) == ["c", "b", "a"]

main! = || {
    result = rev(["a", "b", "c"])
    Stdout.line!(Str.inspect(result))
}
