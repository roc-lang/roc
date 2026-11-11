BoxyStoredLocalMethodClosure :: [].{}

# The stored encoder is a closure of a function the checker promotes out of
# the body, built by the body's caller through the body's captured codec
# requirement; the format's methods stay local procedures of the body.

make_encoder : a -> ({ count : U64, name : Str }, U64 -> Try(U64, []))
make_encoder = |_x| {
	LocalFormat := [Default].{
		rename_field : LocalFormat, Str -> Str
		rename_field = |_, name| name

		encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
		encode_record = |state, _, write_fields| {
			finished = write_fields(
				state + 1,
				|field_state, name, write_value| write_value(field_state + Str.count_utf8_bytes(name)),
			)?
			Ok(finished + 2)
		}

		encode_str : Str, U64 -> Try(U64, [])
		encode_str = |value, state| {
			empty : List(a)
			empty = []
			Ok(state + Str.count_utf8_bytes(value) + List.len(empty) + 100)
		}

		encode_u64 : U64, U64 -> Try(U64, [])
		encode_u64 = |value, state| Ok(state + value)
	}
	encoder_of : value -> (value, U64 -> Try(U64, []))
		where [
			value.encoder_for : LocalFormat -> (value, U64 -> Try(U64, [])),
		]
	encoder_of = |_sample| {
		Shape : value
		|v, st| Shape.encoder_for(LocalFormat.Default)(v, st)
	}
	sample_name : Str
	sample_name = ""
	encoder_of({ count: 0.U64, name: sample_name })
}

stored_encoder = make_encoder(5.U64)

expect {
	name : Str
	name = "abc"
	stored_encoder({ count: 7, name }, 0) == Ok(122)
}

# The body builds the encoder dictionary itself (nothing about the record
# escapes through the body's type), and the stored closure captures that
# dictionary, which captures the format's local procedures.
make_counter : a -> (U64 -> Try(U64, []))
make_counter = |_x| {
	LocalFormat := [Default].{
		rename_field : LocalFormat, Str -> Str
		rename_field = |_, name| name

		encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
		encode_record = |state, _, write_fields| {
			finished = write_fields(
				state + 1,
				|field_state, name, write_value| write_value(field_state + Str.count_utf8_bytes(name)),
			)?
			Ok(finished + 2)
		}

		encode_str : Str, U64 -> Try(U64, [])
		encode_str = |value, state| {
			empty : List(a)
			empty = []
			Ok(state + Str.count_utf8_bytes(value) + List.len(empty) + 100)
		}

		encode_u64 : U64, U64 -> Try(U64, [])
		encode_u64 = |value, state| Ok(state + value)
	}
	counter_of : value -> (U64 -> Try(U64, []))
		where [
			value.encoder_for : LocalFormat -> (value, U64 -> Try(U64, [])),
		]
	counter_of = |sample| {
		Shape : value
		|st| Shape.encoder_for(LocalFormat.Default)(sample, st)
	}
	name : Str
	name = "abc"
	counter_of({ count: 7.U64, name })
}

stored_counter = make_counter(5.U64)

expect stored_counter(0) == Ok(122)
