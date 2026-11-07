LocalFormatSignatureNamesBodyVar :: [].{}

Apples := [Apples(U64)].{
	sample : () -> Apples
	sample = || Apples.Apples(3)
}

Pears := [Pears(Str, Str)].{
	sample : () -> Pears
	sample = || Pears.Pears("green", "pear")
}

# The builder declares a format type with a parameter of its own, and its
# `rename_field` signature applies that parameter to the builder's type
# variable. The method's body builds a value of that variable through the
# builder's `where` clause and lengthens each field name by its inspected size.
make_encoder : a -> ({ count : U64, name : Str }, U64 -> Try(U64, []))
	where [a.sample : () -> a]
make_encoder = |_x| {
	LocalFormat(p) := [Default].{
		rename_field : LocalFormat(a), Str -> Str
		rename_field = |_, name| {
			Outer : a
			made : a
			made = Outer.sample()
			Str.concat(name, Str.repeat("x", Str.count_utf8_bytes(Str.inspect(made))))
		}

		encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
		encode_record = |state, _, write_fields| {
			finished = write_fields(
				state + 1,
				|field_state, name, write_value| write_value(field_state + Str.count_utf8_bytes(name)),
			)?
			Ok(finished + 2)
		}

		encode_str : Str, U64 -> Try(U64, [])
		encode_str = |value, state| Ok(state + Str.count_utf8_bytes(value))

		encode_u64 : U64, U64 -> Try(U64, [])
		encode_u64 = |value, state| Ok(state + value)
	}
	format : LocalFormat(a)
	format = LocalFormat.Default
	encoder_of : value -> (value, U64 -> Try(U64, []))
		where [
			value.encoder_for : LocalFormat(a) -> (value, U64 -> Try(U64, [])),
		]
	encoder_of = |_sample| {
		Shape : value
		Shape.encoder_for(format)
	}
	sample_name : Str
	sample_name = ""
	encoder_of({ count: 0.U64, name: sample_name })
}

encode : ({ count : U64, name : Str }, U64 -> Try(U64, [])) -> Try(U64, [])
encode = |encoder| {
	name : Str
	name = "abc"
	encoder({ count: 7, name }, 0)
}

# 22 for the record, plus each of the two field names lengthened by the
# inspected sample: `Apples(3)` is 9 bytes, `Pears("green", "pear")` 22.
expect encode(make_encoder(Apples.sample())) == Ok(40)
expect encode(make_encoder(Pears.sample())) == Ok(66)
