LiftedUseLeavesOtherUsesRejected :: [].{}

# Annotated records and tag unions are never used as named types, so a use
# relating an annotated result to a nominal type is a type mismatch. An
# unannotated function's result lifts at one use; the other use keeps the
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

Point : { count : U64 }

make : U64 -> { count : U64 }
make = |n| { count: n }

lifted_record : Counter
lifted_record = make(1)

make_point : U64 -> Point
make_point = |n| { count: n }

lifted_point : Counter
lifted_point = make_point(1)

pick : U64 -> [Red, Green]
pick = |n| if n == 0 Red else Green

lifted_color : Color
lifted_color = pick(1)

unit_annotated : U64 -> {}
unit_annotated = |_| {}

lifted_token_annotated : Token
lifted_token_annotated = unit_annotated(1)

lift_a : U64 -> Point
lift_a = |n| {
	c : Counter
	c = { count: n }
	c
}

make_inferred = |_n| { count: 1.U64 }

lifted_inferred : Counter
lifted_inferred = make_inferred(1)

plain_inferred = make_inferred(2)

pick_inferred = |n| if n == 0 Red else Green

lifted_color_inferred : Color
lifted_color_inferred = pick_inferred(1)

plain_color_inferred = pick_inferred(2)

unit = |_n| {}

lifted_token : Token
lifted_token = unit(1)

plain_token = unit(2)

expect plain_inferred.describe() == "counter"
expect plain_color_inferred.describe() == "color"
expect plain_token.describe() == "token"
