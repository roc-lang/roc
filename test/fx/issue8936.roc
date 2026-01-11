app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Issue 8936: Recursive pattern matching with list rest operator (..)
# would panic with "assertion failed: list_resolved.desc.content == .structure"
# when the list type is polymorphic.
#
# Using concrete types to avoid a separate bug with polymorphic function
# type checking (see issue8897.roc comments).

nth : List(Str), U64 -> Try(Str, [OutOfBounds])
nth = |l, i| {
    match (l, i) {
        ([], _) => Err(OutOfBounds)
        ([e, ..], 0) => Ok(e)
        ([_, .. as rest], _) => nth(rest, (i - 1))
    }
}

expect nth(["a", "b", "c", "d", "e"], 2) == Ok("c")
expect nth(["a"], 2) == Err(OutOfBounds)

main! = || {
    Stdout.line!("Tests passed!")
}
