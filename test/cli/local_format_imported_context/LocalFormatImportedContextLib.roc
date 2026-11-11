LocalFormatImportedContextLib :: [].{
	# A builder that declares a format type whose string method reads the
	# builder's type variable, and returns an encoder of a record through it.
	# An importing module calls it at two types, so each call's encoder uses
	# its own instantiation of the format's methods.
	make_promoted : a -> ({ count : U64, name : Str }, U64 -> Try(U64, []))
		where [a.sample : () -> a]
	make_promoted = |_x| {
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
				Outer : a
				made : a
				made = Outer.sample()
				items : List(a)
				items = [made, Outer.sample()]
				Ok(state + Str.count_utf8_bytes(value) + Str.count_utf8_bytes(Str.inspect(items)))
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

	# A builder that declares a format type whose string method reads the
	# builder's type variable and asks, through its own `where` clause, for
	# the encoder of its state. The state is the record being encoded, so
	# the record's generated encoder calls a method that calls that same
	# encoder.
	make_local : a -> ({ name : Str }, { name : Str } -> Try({ name : Str }, [Nested]))
	make_local = |_x| {
		LocalFormat := [Default].{
			rename_field : LocalFormat, Str -> Str
			rename_field = |_, name| name

			encode_record : s, U64, (s, (s, Str, (s -> Try(s, [Nested])) -> Try(s, [Nested])) -> Try(s, [Nested])) -> Try(s, [Nested])
			encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

			encode_str : Str, s -> Try(s, [Nested])
				where [s.encoder_for : LocalFormat -> (s, s -> Try(s, [Nested]))]
			encode_str = |value, state| {
				empty : List(a)
				empty = []
				if value == "recurse" {
					Shape : s
					Shape.encoder_for(LocalFormat.Default)(state, state)
				} else if Str.concat(value, Str.repeat("!", List.len(empty))) == "ab" {
					Err(Nested)
				} else {
					Ok(state)
				}
			}
		}
		encoder_of : value -> (value, { name : Str } -> Try({ name : Str }, [Nested]))
			where [
				value.encoder_for : LocalFormat -> (value, { name : Str } -> Try({ name : Str }, [Nested])),
			]
		encoder_of = |_sample| {
			Shape : value
			|v, st| Shape.encoder_for(LocalFormat.Default)(v, st)
		}
		sample_name : Str
		sample_name = ""
		encoder_of({ name: sample_name })
	}
}
