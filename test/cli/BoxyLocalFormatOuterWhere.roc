BoxyLocalFormatOuterWhere :: [].{
}

# A format type declared in a function body whose methods dispatch on the
# body's type variable through the body's `where` clause. The body's caller
# supplies the body's captured codec requirement, whose generated encoder
# calls those methods.

Apples := [Apples(U64)].{
	sample : () -> Apples
	sample = || Apples.Apples(3)
}

Pears := [Pears(Str, Str)].{
	sample : () -> Pears
	sample = || Pears.Pears("green", "pear")
}

# The encoder comes from a function the checker promotes out of the body, so
# the format's methods reach it through the body's declaration context.
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

		# Reads the declaring function's type variable: builds values of it
		# through its own dispatch, keeps them in a list, inspects them, and
		# does the same in a nested function.
		encode_str : Str, U64 -> Try(U64, [])
		encode_str = |value, state| {
			Outer : a
			made : a
			made = Outer.sample()
			items : List(a)
			items = [made, Outer.sample()]
			measure = |n| {
				inner : a
				inner = Outer.sample()
				n + Str.count_utf8_bytes(Str.inspect(inner))
			}
			Ok(measure(state + Str.count_utf8_bytes(value) + Str.count_utf8_bytes(Str.inspect(items))))
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

# The body that declares the format builds the encoder through one of its own
# procedures.
make_direct : a -> ({ count : U64, name : Str }, U64 -> Try(U64, []))
	where [a.sample : () -> a]
make_direct = |_x| {
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

		# Reads the declaring function's type variable: builds values of it
		# through its own dispatch, keeps them in a list, inspects them, and
		# does the same in a nested function.
		encode_str : Str, U64 -> Try(U64, [])
		encode_str = |value, state| {
			Outer : a
			made : a
			made = Outer.sample()
			items : List(a)
			items = [made, Outer.sample()]
			measure = |n| {
				inner : a
				inner = Outer.sample()
				n + Str.count_utf8_bytes(Str.inspect(inner))
			}
			Ok(measure(state + Str.count_utf8_bytes(value) + Str.count_utf8_bytes(Str.inspect(items))))
		}

		encode_u64 : U64, U64 -> Try(U64, [])
		encode_u64 = |value, state| Ok(state + value)
	}
	# Naming the body's type variable keeps this function a procedure of the
	# body, so the encoder is built by the body that declares the format.
	encoder_of : value -> (value, U64 -> Try(U64, []))
		where [
			value.encoder_for : LocalFormat -> (value, U64 -> Try(U64, [])),
		]
	encoder_of = |_sample| {
		_unused : List(a)
		_unused = []
		Shape : value
		Shape.encoder_for(LocalFormat.Default)
	}
	sample_name : Str
	sample_name = ""
	encoder_of({ count: 0.U64, name: sample_name })
}

# The stored value is a lambda of the promoted function, which encodes when
# called.
make_lambda : a -> ({ count : U64, name : Str }, U64 -> Try(U64, []))
	where [a.sample : () -> a]
make_lambda = |_x| {
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

		# Reads the declaring function's type variable: builds values of it
		# through its own dispatch, keeps them in a list, inspects them, and
		# does the same in a nested function.
		encode_str : Str, U64 -> Try(U64, [])
		encode_str = |value, state| {
			Outer : a
			made : a
			made = Outer.sample()
			items : List(a)
			items = [made, Outer.sample()]
			measure = |n| {
				inner : a
				inner = Outer.sample()
				n + Str.count_utf8_bytes(Str.inspect(inner))
			}
			Ok(measure(state + Str.count_utf8_bytes(value) + Str.count_utf8_bytes(Str.inspect(items))))
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

encode : ({ count : U64, name : Str }, U64 -> Try(U64, [])) -> Try(U64, [])
encode = |encoder| {
	name : Str
	name = "abc"
	encoder({ count: 7, name }, 0)
}

# 22 for the record's fields and bookkeeping, plus the inspected list of two
# samples and the inspected sample its nested function measures:
# `[Apples(3), Apples(3)]` and `Apples(3)`.
apples_expected : Try(U64, [])
apples_expected = Ok(53)

# `[Pears("green", "pear"), Pears("green", "pear")]` and `Pears("green", "pear")`.
pears_expected : Try(U64, [])
pears_expected = Ok(89)

expect encode(make_promoted(Apples.sample())) == apples_expected
