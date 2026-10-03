# A top-level annotated value whose annotation opens an output row implicitly
# generalizes that row (design.md "Polarity"), so each use instantiates it
# fresh. `boom` is used at its own row and at two different wider rows in which
# `Boom` has a different discriminant (`[Boom, Zed]`, `[Aa, Ab, Boom]`), and
# `missing` is used at a wider error row whose other tag carries a payload.
# Every specialization must lower the value at its own layout.
GeneralizedValueWidenedUses := {}

boom : [Boom]
boom = Boom

missing : Try(U64, [Missing])
missing = Err(Missing)

own : [Boom] -> Str
own = |tag| match tag {
    Boom => "boom"
}

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

expect {
    own(boom) == "boom"
    and describe_first(first(Bool.False)) == "boom-first"
    and describe_first(first(Bool.True)) == "zed"
    and describe_last(last(Bool.False)) == "boom-last"
    and describe_last(last(Bool.True)) == "ab"
    and describe_lookup(lookup(Bool.False)) == "missing"
    and describe_lookup(lookup(Bool.True)) == "bad"
}
