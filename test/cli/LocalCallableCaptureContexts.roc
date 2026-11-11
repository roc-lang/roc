LocalCallableCaptureContexts :: [].{}

# Mutually recursive methods retain the declaration's type context through
# their capture edges. Each call creates the same declarations at another type.
parity : a, U64 -> Bool
parity = |_sample, n| {
	Local := [].{
		even : U64 -> Bool
		even = |count| {
			context : List(a)
			context = []
			if count == 0 List.len(context) == 0 else odd(count - 1)
		}

		odd : U64 -> Bool
		odd = |count| if count == 0 count != 0 else even(count - 1)
	}
	Local.even(n)
}

expect parity(1.U64, 8)
expect parity("different context", 7) == (0 == 1)

# These closures share code, but their invocation-specific captures must never
# be folded into one immutable callable.
make_offset : U64 -> (U64 -> U64)
make_offset = |offset| |value| value + offset

first = make_offset(3)
second = make_offset(19)
expect first(7) == 10
expect second(7) == 26
expect {
	closures = [make_offset(5), make_offset(11)]
	List.map(closures, |apply| apply(2)) == [7, 13]
}
