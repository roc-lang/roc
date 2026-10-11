# A top-level value is evaluated once, at its own type, and every use may use
# it at a wider union (design.md "Value Rows: Local Values Share, Top-Level
# Values Widen At Each Use"). `boom` and `bare` are read at `[Boom, Zed]` and
# at `[Aa, Ab, Boom]`, in which `Boom` has a different discriminant; `missing`'s
# error row widens inside `Try` at two uses; and a use at closed unions the
# other uses do not share does not affect them, in either source order.
TopLevelValueWidenedUses := {}

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

boom : [Boom]
boom = Boom

bare = Boom

missing : Try(U64, [Missing])
missing = Err(Missing)

nested : [Wrap([Inner])]
nested = Wrap(Inner)

describe_nested : [Wrap([Inner, Outer]), Plain] -> Str
describe_nested = |tag| match tag {
    Wrap(Inner) => "inner"
    Wrap(Outer) => "outer"
    Plain => "plain"
}

expect describe_last(boom) == "boom-last"
expect describe_first(boom) == "boom-first"
expect describe_first(bare) == "boom-first"
expect describe_last(bare) == "boom-last"
expect describe_lookup(missing) == "missing"
expect describe_other(missing) == "missing-other"
expect describe_nested(nested) == "inner"
expect "${describe_first(boom)} ${describe_last(boom)}" == "boom-first boom-last"

# A single tag whose payloads are all zero-sized has no storage of its own, so
# widening it writes the target's discriminant and payloads from the tag alone:
# into a union whose payloads are zero-sized too (a bare discriminant), into
# one whose payload row widens, and into one whose payload holds `{}`.
describe_outer : [Wrap([Inner]), Plain] -> Str
describe_outer = |tag| match tag {
    Wrap(Inner) => "inner"
    Plain => "plain"
}

describe_wrapped : [Wrap([Inner, Outer])] -> Str
describe_wrapped = |tag| match tag {
    Wrap(Inner) => "inner"
    Wrap(Outer) => "outer"
}

unit_payload : [Wrap({})]
unit_payload = Wrap({})

describe_unit : [Wrap({}), Plain] -> Str
describe_unit = |tag| match tag {
    Wrap(_) => "wrap"
    Plain => "plain"
}

expect describe_outer(nested) == "inner"
expect describe_wrapped(nested) == "inner"
expect describe_unit(unit_payload) == "wrap"
