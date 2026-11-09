# Each `if` below has a number where its condition needs a `Bool`, so the
# condition is a runtime error and the `if` crashes before choosing a branch.
# The `match` over that `if` therefore never sees a value: it evaluates the
# condition and crashes there, whatever shape the branches have.
Widget := [Empty, Count(U64)].{
	current : Str
	current = match if 1 Count(2) else Empty {
		Empty => "empty"
		Count(_) => "count"
	}
}

describe : U64 -> Str
describe = |n| match if n Count(n) else Empty {
	Empty => "empty"
	Count(_) => "count"
}

main! = |_| {
	echo!("before")
	echo!(describe(3))
	echo!(Widget.current)
	Ok({})
}
