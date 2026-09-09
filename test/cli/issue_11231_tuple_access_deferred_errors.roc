bad_index : Bool -> U64
bad_index = |run| if run (|t| t.2)((1, 2)) else 42

bad_shape : Bool -> U64
bad_shape = |run| if run (|t| t.0)({ value: 1 }) else 43

pair : (Str, U64)
pair = ("hello", 44)

bad_element : Bool -> U64
bad_element = |run| if run (|t| t.0)(pair) else pair.1

main! = |_| {
	expect bad_index(False) == 42
	expect bad_shape(False) == 43
	expect bad_element(False) == 44
	Ok(echo!("tuple recovery ran"))
}

expect bad_index(False) == 42
expect bad_shape(False) == 43
expect bad_element(False) == 44
