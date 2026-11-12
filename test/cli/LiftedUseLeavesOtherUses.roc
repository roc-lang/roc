LiftedUseLeavesOtherUses :: [].{}

# One use of each function lifts its anonymous result into a nominal type;
# the other use keeps the anonymous type, and dispatches through it.

Custom := { count : U64, name : Str }.{
	encoder_for = |_encoding| |_value, state| Ok(state + 1000)
}

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
			Ok(state + Str.count_utf8_bytes(value) + List.len(empty))
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

Counter := { count : U64 }.{
	describe : Counter -> Str
	describe = |c| Str.concat("counter ", c.count.to_str())
}

make : U64 -> { count : U64 }
make = |n| { count: n }

lifted_counter : Counter
lifted_counter = make(1)

plain_counter = make(2)

expect lifted_counter.describe() == "counter 1"

expect Str.inspect(plain_counter) == "{ count: 2 }"

plain = make_encoder(5.U64)

lifted : (Custom, U64 -> Try(U64, []))
lifted = make_encoder(5.U64)

expect {
	name : Str
	name = "abc"
	plain({ count: 7, name }, 0) == Ok(22)
}

expect {
	name : Str
	name = "abc"
	lifted(Custom.{ count: 7, name }, 0) == Ok(1000)
}

Token := {}.{
	describe : Token -> Str
	describe = |_| "token"
}

unit = |_n| {}

lifted_token : Token
lifted_token = unit(1)

plain_token = unit(2)

expect lifted_token.describe() == "token"

expect Str.inspect(plain_token) == "{}"

Light := [Red, Green].{
	describe : Light -> Str
	describe = |light|
		match light {
			Red => "red light"
			Green => "green light"
		}
}

signals : U64 -> List([Red, Green])
signals = |_n| [Red]

lifted_signals : List(Light)
lifted_signals = signals(1)

plain_signals = signals(2)

expect lifted_signals.map(Light.describe) == ["red light"]

expect Str.inspect(plain_signals) == "[Red]"
