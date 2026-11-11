LocalFormatMethodRequiresOwnRecord :: [].{}

Rec : { name : Str }

# A format type declared in a function body whose string method asks, through
# its own `where` clause, for the encoder of its state. The state is the record
# being encoded, so the record's generated encoder calls a method that calls
# that same encoder.
make_local : a -> (Rec, Rec -> Try(Rec, [Nested]))
make_local = |_x| {
	LocalFormat := [Default].{
		rename_field : LocalFormat, Str -> Str
		rename_field = |_, name| name

		encode_record : s, U64, (s, (s, Str, (s -> Try(s, [Nested])) -> Try(s, [Nested])) -> Try(s, [Nested])) -> Try(s, [Nested])
		encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

		# Encodes the state with the state's own encoder when asked to,
		# and reports a string equal to "ab" as nested.
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
	encoder_of : value -> (value, Rec -> Try(Rec, [Nested]))
		where [
			value.encoder_for : LocalFormat -> (value, Rec -> Try(Rec, [Nested])),
		]
	encoder_of = |_sample| {
		Shape : value
		|v, st| Shape.encoder_for(LocalFormat.Default)(v, st)
	}
	sample_name : Str
	sample_name = ""
	encoder_of({ name: sample_name })
}

# The same format declared at the top level.
TopFormat := [Default].{
	rename_field : TopFormat, Str -> Str
	rename_field = |_, name| name

	encode_record : s, U64, (s, (s, Str, (s -> Try(s, [Nested])) -> Try(s, [Nested])) -> Try(s, [Nested])) -> Try(s, [Nested])
	encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

	encode_str : Str, s -> Try(s, [Nested])
		where [s.encoder_for : TopFormat -> (s, s -> Try(s, [Nested]))]
	encode_str = |value, state| {
		if value == "recurse" {
			Shape : s
			Shape.encoder_for(TopFormat.Default)(state, state)
		} else if value == "ab" {
			Err(Nested)
		} else {
			Ok(state)
		}
	}
}

encode_top : value, Rec -> Try(Rec, [Nested])
	where [value.encoder_for : TopFormat -> (value, Rec -> Try(Rec, [Nested]))]
encode_top = |value, state| {
	Shape : value
	Shape.encoder_for(TopFormat.Default)(value, state)
}

expect make_local(5.U64)({ name: "x" }, { name: "ab" }) == Ok({ name: "ab" })

# Encoding "recurse" encodes the state, whose name is "ab".
expect make_local(5.U64)({ name: "recurse" }, { name: "ab" }) == Err(Nested)

expect make_local(5.U64)({ name: "recurse" }, { name: "cd" }) == Ok({ name: "cd" })

expect {
	name : Str
	name = "x"
	encode_top({ name }, { name: "ab" }) == Ok({ name: "ab" })
}

expect {
	name : Str
	name = "recurse"
	encode_top({ name }, { name: "ab" }) == Err(Nested)
}
