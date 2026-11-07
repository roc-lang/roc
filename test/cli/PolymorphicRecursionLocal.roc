## An annotated local function recurses at closed types exactly like a
## top-level one.
PolymorphicRecursionLocal := [].{
	concrete : U64 -> U64
	concrete = |n| {
		count : a, U64 -> U64
		count = |_x, k| if k == 0 { 0 } else { 1 + count(1.U8, k - 1) }
		count("s", n)
	}

	enclosing : a, U64 -> U64
	enclosing = |outer, n| {
		go : a, b, U64 -> U64
		go = |x, _y, k| if k == 0 { 0 } else { 1 + go(x, "s", k - 1) }
		go(outer, 1.U8, n)
	}
}

expect PolymorphicRecursionLocal.concrete(4) == 4
expect PolymorphicRecursionLocal.enclosing({}, 5) == 5
