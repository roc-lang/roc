## Structural equality and hashing whose component is a nominal declared in a
## function body with its own `is_eq` / `to_hash`. The component's selected
## method exists only in that block, reached through the comparison's recorded
## derivation edge, so the comparison is not hoisted out of the block.
LocalNominalComponentEvidence := {}

eq_record : Str -> Bool
eq_record = |p| {
    Loc := [L, M].{
        is_eq : Loc, Loc -> Bool
        is_eq = |_, _| Bool.True
    }
    x = { k: Loc.L, n: 1.I64 } == { k: Loc.M, n: 1.I64 }
    x and p == "p"
}

eq_tuple : Str -> Bool
eq_tuple = |p| {
    Loc := [L, M].{
        is_eq : Loc, Loc -> Bool
        is_eq = |_, _| Bool.True
    }
    x = (Loc.L, 1.I64) == (Loc.M, 1.I64)
    x and p == "p"
}

eq_tag : Str -> Bool
eq_tag = |p| {
    Loc := [L, M].{
        is_eq : Loc, Loc -> Bool
        is_eq = |_, _| Bool.True
    }
    x = Wrap(Loc.L) == Wrap(Loc.M)
    x and p == "p"
}

hash_record : Str -> U64
hash_record = |p| {
    Loc := [L, M].{
        is_eq : Loc, Loc -> Bool
        is_eq = |_, _| Bool.True
        to_hash : Loc, Hasher -> Hasher
        to_hash = |_, hasher| Hasher.write_u64(hasher, 0)
    }
    n = Dict.empty().insert({ k: Loc.L }, 1.I64).insert({ k: Loc.M }, 2.I64).len()
    n + Str.count_utf8_bytes(p)
}

expect eq_record("p")
expect eq_tuple("p")
expect eq_tag("p")
expect hash_record("p") == 2
