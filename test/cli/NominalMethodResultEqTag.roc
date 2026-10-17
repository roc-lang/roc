# Equality on a generic function's result whose record field holds a type with
# its own `is_eq` calls that method, including when the other side is a tag the
# result can never equal.
Foo := {}.{
	get = |_| Ok({ flag: Bool.True })
}

expect Foo.get(1) == Err(Bad)

Always := [A, B].{
	is_eq = |_, _| Bool.True
}

always = |_| Ok({ flag: Always.A })

expect always(1) == Ok({ flag: Always.B })
