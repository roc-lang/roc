LocalClosureCallsBodyTypeMethod :: [].{}

Apples := [Apples(U64)].{
	sample : () -> Apples
	sample = || Apples.Apples(3)
}

# A local closure calling a method of a type declared in the same body lists
# that method among its captures. `size` captures nothing and is promoted to a
# top-level procedure.
promoted : Str -> U64
promoted = |s| {
	Fmt := [Fmt].{
		twice : Str -> Str
		twice = |x| Str.concat(x, x)

		size : Str -> U64
		size = |x| Str.count_utf8_bytes(twice(x))
	}
	extra = Str.count_utf8_bytes(s)
	measure = |t| Fmt.size(t) + extra
	measure("ab")
}

# Here `size` calls a sibling method naming the body's type variable, so both
# stay local procedures of the body.
local : a, Str -> U64 where [a.sample : () -> a]
local = |_x, s| {
	Fmt := [Fmt].{
		twice : Str -> Str
		twice = |x| {
			Outer : a
			made : a
			made = Outer.sample()
			Str.concat(x, Str.inspect(made))
		}

		size : Str -> U64
		size = |x| Str.count_utf8_bytes(twice(x))
	}
	extra = Str.count_utf8_bytes(s)
	measure = |t| Fmt.size(t) + extra
	measure("ab")
}

expect promoted("xyz") == 7
expect local(Apples.sample(), "xyz") == 14
