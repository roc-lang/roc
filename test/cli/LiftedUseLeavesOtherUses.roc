LiftedUseLeavesOtherUses :: [].{}

# One use of each unannotated function lifts its anonymous result into a
# nominal type; the other use keeps the anonymous type.

Counter := { count : U64 }.{
	describe : Counter -> Str
	describe = |c| Str.concat("counter ", c.count.to_str())
}

make = |n| { count: n }

lifted_counter : Counter
lifted_counter = make(1)

plain_counter = make(2.U64)

expect lifted_counter.describe() == "counter 1"

expect Str.inspect(plain_counter) == "{ count: 2 }"

Token := {}.{
	describe : Token -> Str
	describe = |_| "token"
}

unit = |_n| {}

lifted_token : Token
lifted_token = unit(1)

plain_token = unit(2)

expect lifted_token.describe() == "token"

expect Str.inspect(plain_token) == "{}"

Light := [Red, Green].{
	describe : Light -> Str
	describe = |light|
		match light {
			Red => "red light"
			Green => "green light"
		}
}

signals = |_n| [Red]

lifted_signals : List(Light)
lifted_signals = signals(1)

plain_signals = signals(2)

expect lifted_signals.map(Light.describe) == ["red light"]

expect Str.inspect(plain_signals) == "[Red]"

# A union an exhaustive match closed still lifts at each call.
is_red = |light|
	match light {
		Red => Bool.True
		Green => Bool.False
	}

named : Light
named = Red

expect is_red(named)

expect is_red(Green) == Bool.False
