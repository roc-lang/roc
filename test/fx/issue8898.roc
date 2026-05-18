app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Issue 8898: Panic when running test on polymorphic function with for loop
# "thread panic: reached unreachable code" at interpreter.zig
# because the interpreter expects list type to be a concrete structure,
# but with polymorphic list parameters in a for loop context,
# the compile-time type is a flex variable instead.

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
    Stdout.line!("done")
}
