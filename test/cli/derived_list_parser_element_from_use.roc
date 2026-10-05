# Deriving a list parser on a type name takes its element type from the parsed value.
Format := [Default].{
	parse_u8 : Format, {} -> Try({ value : U8, rest : {} }, [Bad])
	parse_u8 = |_, rest| Ok({ value: 7, rest })

	parse_list_start : Format, {} -> Try([Counted({ len : U64, rest : {} }), Uncounted({})], [Bad])
	parse_list_start = |_, rest| Ok(Counted({ len: 2, rest }))

	parse_list_next : Format, {} -> Try([Item({}), Done({})], [Bad])
	parse_list_next = |_, _| Err(Bad)

	parse_list_after_item : Format, {} -> Try([Continue({}), Done({})], [Bad])
	parse_list_after_item = |_, _| Err(Bad)
}

expect {
	parsed : Try({ value : List(U8), rest : {} }, [Bad])
	parsed = (List.parser_for(Format.Default))({})
	parsed == Ok({ value: [7, 7], rest: {} })
}

expect {
	parsed = (List.parser_for(Format.Default))({})
	match parsed {
		Ok({ value, rest: _ }) => value == [7.U8, 7]
		Err(_) => Bool.False
	}
}
