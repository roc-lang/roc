OpenRecordsAddedFieldRejected :: [].{}

# A use may not give a function's open record a field it was built without,
# optional or defaulted, in any position: a use cannot change what a function
# it does not own built, and the rule is the same across modules.

Wide := { a : U64, note ?: Str }.{
	describe : Wide -> Str
	describe = |w| Str.concat(w.a.to_str(), w.?note ?? "-")
}

Cfg := { a : U64, level : U8 ?? 3 }

Sink := {}.{
	take : Sink, { a : U64, note ?: Str } -> U64
	take = |_, r| r.a
}

sink : Sink
sink = {}

make = |n| { a: n }

apply = |f| f({ a: 1 })

get_a = |{ a }| a

give = |s, n| s.take({ a: n })

as_structural : { a : U64, note ?: Str }
as_structural = make(1)

as_optional_nominal : Wide
as_optional_nominal = make(2)

as_defaulted_nominal : Cfg
as_defaulted_nominal = make(3)

as_callback = apply(Wide.describe)

wide : Wide
wide = { a: 4 }

as_parameter = get_a(wide)

structural_value : { a : U64, note ?: Str }
structural_value = { a: 5 }

as_structural_parameter = get_a(structural_value)

in_method = give(sink, 6)

built : Wide
built = Wide.{ a: 7 }

expect built.describe() == "7-"
