OperatorMethodMismatch :: [].{}

# An operator needs its method at one type on both sides: `<` needs
# `is_lt : a, a -> Bool`, and `*` needs `times : a, a -> a`. Each type below
# declares the method at another type, calls it by name, and then uses the
# operator. The operator use is an error; the call by name stays valid.
Ranked := [Ranked].{
	is_lt : Ranked, Ranked -> Str
	is_lt = |_, _| "no"
}

ranked : Ranked
ranked = Ranked

by_name = ranked.is_lt(ranked)

by_operator = ranked < ranked

Duration := { millis : I64 }.{
	times : Duration, I64 -> Duration
	times = |duration, scale| { millis: duration.millis * scale }
}

duration : Duration
duration = { millis: 10 }

scaled = duration.times(3)

squared = duration * duration
