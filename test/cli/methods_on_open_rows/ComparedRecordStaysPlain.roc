# A record a function builds and compares is plain for methods: a use that
# makes it a nominal type is a type mismatch, whether the function built it
# from a literal or a parameter's row joined the literal's.

ComparedRecordStaysPlain :: [].{}

Tagged := { id : U64 }.{
	is_eq : Tagged, Tagged -> Bool
	is_eq = |_, _| Bool.True
}

one : Tagged
one = { id: 1 }

eq_only = |a, b| {
	left = { id: a }
	right = { id: b }
	(left == right, (left, right))
}

keep_eq = |r, n| {
	other = if n == 0 r else { id: n }
	(r == other, other)
}

pair : (Tagged, Tagged)
pair = eq_only(1, 2).1

kept = keep_eq(one, 2)
