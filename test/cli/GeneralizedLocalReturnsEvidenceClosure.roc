# A generalized local function, called with its hidden evidence, that
# builds or returns a local closure reading evidence of its own or of the
# enclosing function.
GeneralizedLocalReturnsEvidenceClosure :: [].{}

W(a) := a.{
	encoder_for : _
}

# The returned closure reads the enclosing function's encoder and the local's
# own `plus` and numeral.
encoded = |r| {
	h = |n| {
		g = |m| Json.to_str(W.(r)).concat(Str.inspect(n + m))
		g
	}
	h(1.U8)
}

# A closure that reads its enclosing local's numeral, called inside it.
numeral_inside = |r| {
	h = |n| {
		g = |s| s.concat(Str.inspect(n + 1))
		g(r.to_str())
	}
	h(1.U8)
}

# A monomorphic closure inside a generalized local reads the enclosing
# function's `to_str`.
monomorphic_inside = |r| {
	h = |_n| {
		g = |s| Str.concat(r.to_str(), s)
		g
	}
	h(1.U8)
}

# The returned closure reads the enclosing function's `to_str` and its own
# numeral, and is called after the local returns it.
returned_then_called = |r| {
	h = |n| {
		g = |s| Str.concat(r.to_str(), s).concat(Str.inspect(n + 1))
		g
	}
	k = h(1.U8)
	k("x")
}

# A literal whose type a generalized local's scheme quantifies only through
# its copy of a nested local's requirement on the enclosing function's method.
literal_through_requirement = |r| {
	h = |_n| {
		g = |s| r.to_str().concat(s)
		g(".")
	}
	h(1.U8)
}

# The same literal beside the local's own numeral.
literal_and_numeral = |r| {
	h = |n| {
		g = |s| r.to_str().concat(s).concat(Str.inspect(n + 1))
		g(".")
	}
	h(1.U8)
}

expect encoded(7.U64)(2.U8) == "73"
expect literal_through_requirement(7.U64) == "7."
expect literal_and_numeral(7.U64) == "7.2"
expect numeral_inside(7.U64) == "72"
expect monomorphic_inside(7.U64)("x") == "7x"
expect returned_then_called(7.U64) == "7x2"
expect returned_then_called(8.U64) == "8x2"
