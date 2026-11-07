LocalFormatStoresBodyVar :: [].{}

# A format type declared in a function body whose backing stores values of the
# function's type variable, so the format's representation varies with the
# function's instantiation, and the generated encoder holds a format value of
# that representation.
make_encoder : a -> ({ count : U64, name : Str }, U64 -> Try(U64, []))
make_encoder = |x| {
	LocalFormat := [Default, Other(List(a))].{
		rename_field : LocalFormat, Str -> Str
		rename_field = |_, name| Str.concat(name, "!")

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
	encoder_of : value -> (value, U64 -> Try(U64, []))
		where [
			value.encoder_for : LocalFormat -> (value, U64 -> Try(U64, [])),
		]
	encoder_of = |_sample| {
		Shape : value
		Shape.encoder_for(LocalFormat.Default)
	}
	# A local function whose annotation names the format type and no type
	# variable, and which reads the values a format stores.
	stored : LocalFormat -> U64
	stored = |format| match format {
		Default => 0
		Other(items) => List.len(items)
	}
	sample_name : Str
	sample_name = Str.repeat("x", stored(LocalFormat.Other([x, x, x])))
	encoder_of({ count: 0.U64, name: sample_name })
}

encode : ({ count : U64, name : Str }, U64 -> Try(U64, [])) -> Try(U64, [])
encode = |encoder| {
	name : Str
	name = "abc"
	encoder({ count: 7, name }, 0)
}

# 22 for the record, plus the mark on each of the two field names.
expect encode(make_encoder(5.U64)) == Ok(24)
expect encode(make_encoder("x")) == Ok(24)
expect encode(make_encoder([1.U8, 2])) == Ok(24)
