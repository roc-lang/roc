LiftedUseLeavesOtherUsesRejected :: [].{}

# Each function returns an anonymous type, and one use of each lifts the
# result into a nominal type whose backing matches. The other use keeps the
# anonymous type, which has no methods, so calling one on it is an error.

Counter := { count : U64 }.{
	describe : Counter -> Str
	describe = |_| "counter"
}

Color := [Red, Green].{
	describe : Color -> Str
	describe = |_| "color"
}

Token := {}.{
	describe : Token -> Str
	describe = |_| "token"
}

Never := [].{
	describe : Never -> Str
	describe = |_| "never"
}

Point : { count : U64 }

make : U64 -> { count : U64 }
make = |n| { count: n }

lifted_record : Counter
lifted_record = make(1)

plain_record = make(2)

make_point : U64 -> Point
make_point = |n| { count: n }

lifted_point : Counter
lifted_point = make_point(1)

plain_point = make_point(2)

make_inferred = |_n| { count: 1.U64 }

lifted_inferred : Counter
lifted_inferred = make_inferred(1)

plain_inferred = make_inferred(2)

pick : U64 -> [Red, Green]
pick = |n| if n == 0 Red else Green

lifted_color : Color
lifted_color = pick(1)

plain_color = pick(2)

wrap : U64 -> List({ count : U64 })
wrap = |n| [{ count: n }]

lifted_list : List(Counter)
lifted_list = wrap(1)

plain_list = wrap(2)

unit = |_n| {}

lifted_token : Token
lifted_token = unit(1)

plain_token = unit(2)

unit_annotated : U64 -> {}
unit_annotated = |_| {}

lifted_token_annotated : Token
lifted_token_annotated = unit_annotated(1)

plain_token_annotated = unit_annotated(2)

nothing : U64 -> List([])
nothing = |_| []

lifted_never : List(Never)
lifted_never = nothing(1)

plain_never = nothing(2)

# Two annotations naming the same alias each get their own anonymous
# structure, so one definition lifting its result leaves the other's alone.
lift_a : U64 -> Point
lift_a = |n| {
	c : Counter
	c = { count: n }
	c
}

keep_b : U64 -> Point
keep_b = |n| { count: n }

expect plain_record.describe() == "counter"
expect plain_point.describe() == "counter"
expect plain_inferred.describe() == "counter"
expect plain_color.describe() == "color"
expect List.map(plain_list, |c| c.describe()) == ["counter"]
expect plain_token.describe() == "token"
expect plain_token_annotated.describe() == "token"
expect List.map(plain_never, |n| n.describe()) == []
expect keep_b(1).describe() == "counter"
