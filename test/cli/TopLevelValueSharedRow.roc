# An unannotated top-level value's tag rows are shared by every use in its
# module and widen together, then close when the module finishes checking
# (design.md "Value Rows: Uses Share One Row"). `bare` is read where
# `[Boom, Zed, ..]` and `[Aa, Ab, Boom, ..]` are accepted, so its one row ends
# as `[Aa, Ab, Boom, Zed]`, in which `Boom` sits at a different discriminant
# than in `[Boom]`; `missing`'s error row widens inside `Try`, and its ok type
# is decided by the uses; `nested`'s payload row widens through a tag payload.
# An annotated value keeps the annotation's row and is used at exactly that
# row. Nothing converts a value: each value is laid out once at its final row.
TopLevelValueSharedRow := {}

describe_first : [Boom, Zed, ..] -> Str
describe_first = |tag| match tag {
    Boom => "boom-first"
    Zed => "zed"
    _ => "other"
}

describe_last : [Aa, Ab, Boom, ..] -> Str
describe_last = |tag| match tag {
    Aa => "aa"
    Ab => "ab"
    Boom => "boom-last"
    _ => "other"
}

describe_lookup : Try(U64, [Invalid(Str), Missing, ..]) -> Str
describe_lookup = |result| match result {
    Ok(_) => "ok"
    Err(Invalid(msg)) => msg
    Err(Missing) => "missing"
    Err(_) => "other"
}

describe_other : Try(U64, [Missing, Other, ..]) -> Str
describe_other = |result| match result {
    Ok(_) => "ok"
    Err(Missing) => "missing-other"
    Err(Other) => "other"
    Err(_) => "unknown"
}

describe_nested : [Wrap([Inner, Outer, ..]), Plain, ..] -> Str
describe_nested = |tag| match tag {
    Wrap(Inner) => "inner"
    Wrap(Outer) => "outer"
    Wrap(_) => "wrapped"
    Plain => "plain"
    _ => "other"
}

describe_boom : [Boom] -> Str
describe_boom = |tag| match tag {
    Boom => "boom"
}

bare = Boom

missing = Err(Missing)

nested = Wrap(Inner)

boom : [Boom]
boom = Boom

expect describe_first(bare) == "boom-first"
expect describe_last(bare) == "boom-last"
expect describe_lookup(missing) == "missing"
expect describe_other(missing) == "missing-other"
expect describe_nested(nested) == "inner"
expect describe_boom(boom) == "boom"
expect "${describe_first(bare)} ${describe_last(bare)}" == "boom-first boom-last"
