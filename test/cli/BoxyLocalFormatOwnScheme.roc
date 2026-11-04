BoxyLocalFormatOwnScheme :: [].{}

# A format type declared in a function body whose `encode_u64` method has its
# own scheme and `where` clause. The body's caller supplies the body's captured
# codec requirements, whose generated encoders call that method, so its
# function value is built from the body's declaration context with its own
# dictionaries resolved there.

Rec : { name : Str }

Cnt : { count : U64 }

make_rec_first : a -> { rec : (Rec, Rec -> Try(Rec, [])), cnt : (Cnt, Rec -> Try(Rec, [])) }
make_rec_first = |_x| {
	LocalFormat := [Default].{
		rename_field : LocalFormat, Str -> Str
		rename_field = |_, name| name

		encode_record : Rec, U64, (Rec, (Rec, Str, (Rec -> Try(Rec, [])) -> Try(Rec, [])) -> Try(Rec, [])) -> Try(Rec, [])
		encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

		encode_str : Str, Rec -> Try(Rec, [])
		encode_str = |value, state| {
			empty : List(a)
			empty = []
			Ok({ name: Str.concat(Str.concat(state.name, value), Str.repeat("!", List.len(empty))) })
		}

		# Encodes the state with the state's own encoder, through its own
		# `where` clause; naming the body's type variable keeps it a local
		# procedure of the body.
		encode_u64 : U64, s -> Try(s, [])
			where [s.encoder_for : LocalFormat -> (s, s -> Try(s, []))]
		encode_u64 = |_value, state| {
			empty : List(a)
			empty = []
			_unused = List.len(empty)
			Shape : s
			Shape.encoder_for(LocalFormat.Default)(state, state)
		}
	}
	encoder_of : value -> (value, Rec -> Try(Rec, []))
		where [
			value.encoder_for : LocalFormat -> (value, Rec -> Try(Rec, [])),
		]
	encoder_of = |_sample| {
		Shape : value
		|v, st| Shape.encoder_for(LocalFormat.Default)(v, st)
	}
	sample_name : Str
	sample_name = ""
	rec = encoder_of({ name: sample_name })
	cnt = encoder_of({ count: 0.U64 })
	{ rec, cnt }
}

# The same body building its encoders in the other order.
make_cnt_first : a -> { rec : (Rec, Rec -> Try(Rec, [])), cnt : (Cnt, Rec -> Try(Rec, [])) }
make_cnt_first = |_x| {
	LocalFormat := [Default].{
		rename_field : LocalFormat, Str -> Str
		rename_field = |_, name| name

		encode_record : Rec, U64, (Rec, (Rec, Str, (Rec -> Try(Rec, [])) -> Try(Rec, [])) -> Try(Rec, [])) -> Try(Rec, [])
		encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

		encode_str : Str, Rec -> Try(Rec, [])
		encode_str = |value, state| {
			empty : List(a)
			empty = []
			Ok({ name: Str.concat(Str.concat(state.name, value), Str.repeat("!", List.len(empty))) })
		}

		# Encodes the state with the state's own encoder, through its own
		# `where` clause; naming the body's type variable keeps it a local
		# procedure of the body.
		encode_u64 : U64, s -> Try(s, [])
			where [s.encoder_for : LocalFormat -> (s, s -> Try(s, []))]
		encode_u64 = |_value, state| {
			empty : List(a)
			empty = []
			_unused = List.len(empty)
			Shape : s
			Shape.encoder_for(LocalFormat.Default)(state, state)
		}
	}
	encoder_of : value -> (value, Rec -> Try(Rec, []))
		where [
			value.encoder_for : LocalFormat -> (value, Rec -> Try(Rec, [])),
		]
	encoder_of = |_sample| {
		Shape : value
		|v, st| Shape.encoder_for(LocalFormat.Default)(v, st)
	}
	sample_name : Str
	sample_name = ""
	cnt = encoder_of({ count: 0.U64 })
	rec = encoder_of({ name: sample_name })
	{ rec, cnt }
}

expect {
	encoders = make_rec_first(5.U64)
	(encoders.cnt)({ count: 7 }, { name: "ab" }) == Ok({ name: "abab" })
}

expect {
	encoders = make_rec_first(5.U64)
	(encoders.rec)({ name: "x" }, { name: "ab" }) == Ok({ name: "abx" })
}

expect {
	encoders = make_cnt_first(5.U64)
	(encoders.cnt)({ count: 7 }, { name: "ab" }) == Ok({ name: "abab" })
}

expect {
	encoders = make_cnt_first(5.U64)
	(encoders.rec)({ name: "x" }, { name: "ab" }) == Ok({ name: "abx" })
}
