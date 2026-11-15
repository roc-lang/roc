# An entry point bound to a computed callable: compile-time evaluation produces
# the function value `main!` names, and the program's root calls that value
# restored from the constant store. The value captures a record, a list and a
# derived encoder, so restoring it rebuilds each of those in the root's body.
Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name
	encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
	encode_record = |state, _, write_fields| write_fields(state, |field_state, _, write_value| write_value(field_state))
	encode_u64 : U64, U64 -> Try(U64, [Zero])
	encode_u64 = |value, state| if value == 0 { Err(Zero) } else { Ok(state + value) }
}

encoder_of : shape -> (shape, U64 -> Try(U64, err))
	where [shape.encoder_for : Format -> (shape, U64 -> Try(U64, err))]
encoder_of = |_| {
	Shape : shape
	Shape.encoder_for(Format.Default)
}

make_main = |encode, label, table| |args| {
	count = List.len(args)
	echo!("${label} ${Str.inspect(encode({ n: count + 3 }, 4))} ${Str.inspect(encode({ n: count }, 4))} ${Str.inspect(table)}\n")
	Ok({})
}

main! = make_main(encoder_of({ n: 0 }), "enc", { sizes: [1, 2, 3], name: "t" })
