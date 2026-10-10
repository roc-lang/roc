# A self-recursive callable value built by a block, used twice at the same type under one owner: inside one expect, and inside
# another value's inlined lambda body. Each use expands the value under its
# own recursive binding, so the lambda lowered for one use names that use's
# binding and must not be reused for the other.
RecursiveValueRepeatedUses :: [].{}

pick : U64 -> [A, B]
pick = {
	z = 0
	|n| if n == z { A } else if n == 1 { B } else { pick(n - 2) }
}

expect pick(4) == A and pick(5) == B
expect pick(6) == A

both : U64 -> Bool
both = {
	y = 0
	|n| pick(n + y) == A and pick(n + 1) == B
}

expect both(2)
