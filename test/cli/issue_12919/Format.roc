Format := [Default].{
	encode_u64 : U64, U64 -> Try(U64, [])
	encode_u64 = |value, state| Ok(state + value)

	encode_str : Str, U64 -> Try(U64, [])
	encode_str = |value, state| Ok(state + value.count_utf8_bytes())

	encode_tuple : U64, U64, (U64, (U64, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
	encode_tuple = |state, _count, write| write(state, |items, write_item| write_item(items))

	encode_dict : U64, U64, (U64, (U64, (U64 -> Try(U64, [])), (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
	encode_dict = |state, _count, write| write(state, |items, write_key, write_value| write_value(write_key(items)?))

	encode_key_start : Format, U64 -> Try(U64, [])
	encode_key_start = |_, state| Ok(state)
}

# https://github.com/roc-lang/roc/issues/12919
expect {
	Shape : Dict((U64, Str), Str)
	Shape.encoder_for(Format.Default)(Dict.from_list([((1.U64, "a"), "one"), ((2.U64, "b"), "two")]), 0.U64) == Ok(11)
}
