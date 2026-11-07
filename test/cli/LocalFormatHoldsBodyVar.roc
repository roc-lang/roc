LocalFormatHoldsBodyVar :: [].{}

# A format type declared in a function body whose backing stores values of the
# function's type variable, and whose generated encoder holds a format value
# that stores the function's argument: `rename_field` reads how many it
# stores. The encoder requirement in the body's scheme names the format, so
# each caller's instantiation of the body gives the format its own values.
make_encoder : a -> ({ count : U64, name : Str }, U64 -> Try(U64, []))
make_encoder = |x| {
	LocalFormat := [Default, Other(List(a))].{
		rename_field : LocalFormat, Str -> Str
		rename_field = |format, name| match format {
			Default => Str.concat(name, "!")
			Other(items) => Str.concat(name, Str.repeat("?", List.len(items)))
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
	encoder_of : value, LocalFormat -> (value, U64 -> Try(U64, []))
		where [
			value.encoder_for : LocalFormat -> (value, U64 -> Try(U64, [])),
		]
	encoder_of = |_sample, format| {
		Shape : value
		Shape.encoder_for(format)
	}
	format = LocalFormat.Other([x, x, x])
	sample_name : Str
	sample_name = ""
	encoder_of({ count: 0.U64, name: sample_name }, format)
}

encode : ({ count : U64, name : Str }, U64 -> Try(U64, [])) -> Try(U64, [])
encode = |encoder| {
	name : Str
	name = "abc"
	encoder({ count: 7, name }, 0)
}

# 22 for the record, plus three marks on each of the two field names.
expect encode(make_encoder("x")) == Ok(28)
expect encode(make_encoder([1.U8, 2])) == Ok(28)
expect encode(make_encoder(Str.repeat("long enough to live on the heap ", 2))) == Ok(28)
