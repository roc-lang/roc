# Constants an imported module stores in the checked-module cache, holding
# capturing closures inside a list of records and a nested record.
Registry := [].{
    prefixer : Str -> (Str -> Str)
    prefixer = |p| |s| Str.concat(p, s)

    entries : List({ name : Str, render : Str -> Str, weight : U64 -> U64 })
    entries = List.map(["alpha", "beta", "gamma"], |n| { name: n, render: Registry.prefixer(Str.concat(n, ":")), weight: |w| w + Str.count_utf8_bytes(n) })

    table : { lookup : Str -> Str, nested : { inner : U64 -> U64 } }
    table = { lookup: Registry.prefixer("t-"), nested: { inner: |x| x * 3 } }
}
