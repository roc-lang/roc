# A block-local annotated value whose annotation opens an output row implicitly
# generalizes that row exactly like a top-level value (design.md "Polarity"):
# each use instantiates the row fresh. The value is evaluated once into its
# binder and each use widens that one value by a row coercion (design.md "Row
# Coercion Primitive"), so `boom` is read at `[Boom, Zed]` and at
# `[Aa, Ab, Boom]`, in which `Boom` has a different discriminant, `missing`'s
# error row widens inside `Try` at two uses, and a value built by a helper is
# widened at a use whose other tag carries a payload. An unannotated alias of
# such a value, a local function returning one, and an error row whose tag
# carries a function are widened the same way, and an alias parameter that also
# sits under a `List` keeps that row shared.
GeneralizedLocalValueWidenedUses := {}

describe_first : [Boom, Zed] -> Str
describe_first = |tag| match tag {
    Boom => "boom-first"
    Zed => "zed"
}

describe_last : [Aa, Ab, Boom] -> Str
describe_last = |tag| match tag {
    Aa => "aa"
    Ab => "ab"
    Boom => "boom-last"
}

describe_lookup : Try(U64, [Invalid(Str), Missing]) -> Str
describe_lookup = |result| match result {
    Ok(_) => "ok"
    Err(Invalid(msg)) => msg
    Err(Missing) => "missing"
}

describe_other : Try(U64, [Missing, Other]) -> Str
describe_other = |result| match result {
    Ok(_) => "ok"
    Err(Missing) => "missing-other"
    Err(Other) => "other"
}

pick : Bool -> Try(U64, [Missing])
pick = |flag| if flag Ok(7) else Err(Missing)

run : Bool -> Str
run = |flag| {
    boom : [Boom]
    boom = Boom

    missing : Try(U64, [Missing])
    missing = pick(flag)

    first = describe_first(boom)
    last = describe_last(boom)
    lookup = describe_lookup(missing)
    other = describe_other(missing)
    "${first} ${last} ${lookup} ${other}"
}

expect run(Bool.False) == "boom-first boom-last missing missing-other"
expect run(Bool.True) == "boom-first boom-last ok ok"

describe_fn : Try(U64, [Inc(U64 -> U64), Other]) -> U64
describe_fn = |result| match result {
    Ok(n) => n
    Err(Inc(f)) => f(1)
    Err(Other) => 0
}

describe_fn_more : Try(U64, [Again, Inc(U64 -> U64)]) -> U64
describe_fn_more = |result| match result {
    Ok(n) => n
    Err(Again) => 0
    Err(Inc(f)) => f(10)
}

Pair(x) : [Single(x), Many(List(x))]

describe_pair : Pair([Ee, Ff]) -> Str
describe_pair = |pair| match pair {
    Single(Ee) => "single-ee"
    Single(Ff) => "single-ff"
    Many(items) => Str.inspect(List.len(items))
}

run_more : U64 -> Str
run_more = |step| {
    boom : [Boom]
    boom = Boom

    alias = boom
    get = |_| boom

    inc : Try(U64, [Inc(U64 -> U64)])
    inc = if step == 0 Ok(5) else Err(Inc(|n| n + step))

    pair : Pair([Ee])
    pair = Single(Ee)

    aliased = Str.concat(describe_first(alias), describe_last(alias))
    returned = Str.concat(describe_first(get({})), describe_last(get({})))
    called = describe_fn(inc) + describe_fn_more(inc)
    "${aliased} ${returned} ${Str.inspect(called)} ${describe_pair(pair)}"
}

expect run_more(2) == "boom-firstboom-last boom-firstboom-last 15 single-ee"
expect run_more(0) == "boom-firstboom-last boom-firstboom-last 10 single-ee"

add_one : U64 -> U64
add_one = |n| n + 1

classify_err : Try(U8, [E1, E2(U64 -> U64), E3]) -> U64
classify_err = |result| match result {
    Ok(_) => 0
    Err(E1) => 1
    Err(E2(f)) => f(2)
    Err(E3) => 3
}

# The error row holds a callable behind `Try`'s payload box, and the coercion
# into the call argument adapts the value's descriptor (Boxy).
classify_mode : U64 -> U64
classify_mode = |mode| {
    v : Try(U8, [E2(U64 -> U64), E3])
    v = if mode == 0 Err(E2(add_one)) else if mode == 1 Err(E3) else Ok(7)
    classify_err(v)
}

expect classify_mode(0) + classify_mode(1) * 10 + classify_mode(2) * 100 == 33
