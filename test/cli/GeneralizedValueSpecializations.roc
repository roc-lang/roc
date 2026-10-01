# Generalized top-level values are compile-time evaluated once per concrete
# specialization (design.md "Specialization-Owned Top-Level Values"). `boom`
# and `missing` write `..` in an output row, so each use instantiates the row
# fresh and every specialization is its own compile-time value at its own
# layout: `Boom` has a different discriminant in `[Boom, Zed]` and in
# `[Aa, Ab, Boom]`. `made` is used at two element types.
GeneralizedValueSpecializations := {}

boom : [Boom, ..]
boom = Boom

missing : Try(U64, [Missing, ..])
missing = Err(Missing)

grow : U64, List(a) -> List(a)
grow = |n, acc| if n == 0 { acc } else { grow(n - 1, acc) }

made : List(a)
made = grow(3, [])

first : Bool -> [Boom, Zed]
first = |flag| if flag Zed else boom

describe_first : [Boom, Zed] -> Str
describe_first = |tag| match tag {
    Boom => "boom-first"
    Zed => "zed"
}

last : Bool -> [Aa, Ab, Boom]
last = |flag| if flag Ab else boom

describe_last : [Aa, Ab, Boom] -> Str
describe_last = |tag| match tag {
    Aa => "aa"
    Ab => "ab"
    Boom => "boom-last"
}

lookup : Bool -> Try(U64, [Invalid(Str), Missing])
lookup = |flag| if flag Err(Invalid("bad")) else missing

describe_lookup : Try(U64, [Invalid(Str), Missing]) -> Str
describe_lookup = |result| match result {
    Ok(_) => "ok"
    Err(Invalid(msg)) => msg
    Err(Missing) => "missing"
}

with_number : U64 -> List(U64)
with_number = |n| List.append(made, n)

with_word : Str -> List(Str)
with_word = |w| List.append(made, w)

expect {
    describe_first(first(Bool.False)) == "boom-first"
    and describe_first(first(Bool.True)) == "zed"
    and describe_last(last(Bool.False)) == "boom-last"
    and describe_last(last(Bool.True)) == "ab"
    and describe_lookup(lookup(Bool.False)) == "missing"
    and describe_lookup(lookup(Bool.True)) == "bad"
    and with_number(7) == [7]
    and with_word("w") == ["w"]
}
