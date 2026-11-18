Gates := [Count, Reject].{
	rename_field : Gates, Str -> Str
	rename_field = |_, name| name

	encode_u64 : U64, U64 -> Try(U64, [Blocked])
	encode_u64 = |value, state| Ok(state + value)

	encode_str : Str, U64 -> Try(U64, [Blocked])
	encode_str = |value, state| Ok(state + value.count_utf8_bytes())

	encode_tuple : U64, U64, (U64, (U64, (U64 -> Try(U64, [Blocked])) -> Try(U64, [Blocked])) -> Try(U64, [Blocked])) -> Try(U64, [Blocked])
	encode_tuple = |state, _count, write| write(state, |items, write_item| write_item(items))

	encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [Blocked])) -> Try(U64, [Blocked])) -> Try(U64, [Blocked])) -> Try(U64, [Blocked])
	encode_record = |state, _count, write| write(state, |items, _name, write_item| write_item(items))

	encode_dict : U64, U64, (U64, (U64, (U64 -> Try(U64, [Blocked])), (U64 -> Try(U64, [Blocked])) -> Try(U64, [Blocked])) -> Try(U64, [Blocked])) -> Try(U64, [Blocked])
	encode_dict = |state, _count, write| write(state, |items, write_key, write_value| write_value(write_key(items)?))

	encode_key_str : Gates, Str, U64 -> Try(U64, [Blocked])
	encode_key_str = |_, value, state| Gates.encode_str(value, state)

	encode_key_start : Gates, U64 -> Try(U64, [Blocked])
	encode_key_start = |format, state| match format {
		Count => Ok(state + 100)
		Reject => Err(Blocked)
	}
}

Id := U64.{
	encoder_for : _
	is_eq : _
	to_hash : _
}

Wrapped := { inside : Dict((U64, Str), Str) }.{
	encoder_for : _
}

encode : a, Gates -> Try(U64, [Blocked]) where [a.encoder_for : Gates -> (a, U64 -> Try(U64, [Blocked]))]
encode = |value, format| {
	Shape : a
	Shape.encoder_for(format)(value, 0)
}

# One boundary has multiple composite key shapes; each runtime key runs its gate.
expect encode(
	(
		Dict.from_list([((1.U64, "a"), "one"), ((2.U64, "b"), "two")]),
		Dict.from_list([({ n: 3.U64 }, "x")]),
		Dict.from_list([(Id.(4.U64), "y")]),
	),
	Gates.Count,
) == Ok(420)

# A nested generated nominal codec must not inherit or overwrite its parent's gate role.
expect encode(
	(
		Dict.from_list([((1.U64, "a"), "one")]),
		Wrapped.{ inside: Dict.from_list([((2.U64, "b"), "two")]) },
		Dict.from_list([({ n: 3.U64 }, "x")]),
	),
	Gates.Count,
) == Ok(315)

# Errors from the gate stop encoding the key and value.
expect encode(Dict.from_list([((1.U64, "a"), "one")]), Gates.Reject) == Err(Blocked)

# Scalar keys bypass the composite gate, even when it would reject them.
expect encode(Dict.from_list([("a", 1.U64), ("b", 2.U64)]), Gates.Reject) == Ok(5)

Text := [Default].{
	encode_u64 : U64, Str -> Try(Str, [])
	encode_u64 = |_, state| Ok(state.concat("n"))

	encode_tuple : Str, U64, (Str, (Str, (Str -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])
	encode_tuple = |state, _count, write| write(state, |items, write_item| write_item(items))

	encode_dict : Str, U64, (Str, (Str, (Str -> Try(Str, [])), (Str -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])
	encode_dict = |state, _count, write| write(state, |items, write_key, write_value| write_value(write_key(items)?))

	encode_key_start : Text, Str -> Try(Str, [])
	encode_key_start = |_, state| Ok(state.concat("|"))
}

# Another format's state and error types select an independent gate.
expect {
	Shape : Dict((U64, U64), U64)
	Shape.encoder_for(Text.Default)(Dict.from_list([((1.U64, 2.U64), 3.U64)]), "") == Ok("|nnn")
}
