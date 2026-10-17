# One annotated closed value is forwarded into two function results whose
# rows both name `Conflict`, at two different payload types. The joins put
# both `Conflict`s in one row as two variants of one label (design.md "Row
# Joins"): each function's result and each `match` is written at one static
# type, which lists the label once and selects its variant by payload, so a
# value built as `Conflict({ a })` is never read as `Conflict({ b })`. Nothing
# converts the forwarded value.
JoinedRowRepeatedLabel := {}

shared_value : [Shared]
shared_value = Shared

fwd_a : {} -> [Shared, Conflict({ a : U64 })]
fwd_a = |{}| shared_value

fwd_b : {} -> [Shared, Conflict({ b : Str })]
fwd_b = |{}| shared_value

describe_a : [Shared, Conflict({ a : U64 })] -> Str
describe_a = |v| match v {
	Shared => "shared"
	Conflict({ a }) => a.to_str()
}

describe_b : [Shared, Conflict({ b : Str })] -> Str
describe_b = |v| match v {
	Shared => "shared"
	Conflict({ b }) => b
}

expect describe_a(fwd_a({})) == "shared"
expect describe_b(fwd_b({})) == "shared"
expect describe_a(Conflict({ a: 7 })) == "7"
expect describe_b(Conflict({ b: "x" })) == "x"
