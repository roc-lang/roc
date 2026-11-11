LocalFormatImportedLib :: [].{
	# A stored encoder built by a body whose local type's methods are local
	# procedures.
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
			Shape.encoder_for(LocalFormat.Default)
		}
		sample_name : Str
		sample_name = ""
		encoder_of({ count: 0.U64, name: sample_name })
	}

	# Encodes directly in the body that declares the format.
	local_format : a -> Try(U64, [])
	local_format = |_x| {
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
				Ok(state + Str.count_utf8_bytes(value) + List.len(empty))
			}

			encode_u64 : U64, U64 -> Try(U64, [])
			encode_u64 = |value, state| Ok(state + value)
		}
		encode_local : value -> Try(U64, [])
			where [
				value.encoder_for : LocalFormat -> (value, U64 -> Try(U64, [])),
			]
		encode_local = |value| {
			Shape : value
			encode_value = Shape.encoder_for(LocalFormat.Default)
			encode_value(value, 0)
		}
		name : Str
		name = "abc"
		encode_local({ count: 7.U64, name })
	}
}
