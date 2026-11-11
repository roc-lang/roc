LocalFormatOuterTypeVarTwoInstantiations :: [].{}

Apples := [Apples(U64)].{
	sample : () -> Apples
	sample = || Apples.Apples(3)
}

Pears := [Pears(Str, Str)].{
	sample : () -> Pears
	sample = || Pears.Pears("green", "pear")
}

# The builder declares a format type whose string method reads the builder's
# type variable, and returns an encoder of a record through it. The builder is
# called at two types, so each call's encoder must use its own instantiation of
# the format's methods.
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

encode : ({ count : U64, name : Str }, U64 -> Try(U64, [])) -> Try(U64, [])
encode = |encoder| {
	name : Str
	name = "abc"
	encoder({ count: 7, name }, 0)
}

# Each `encode_str` adds the byte length of `Str.inspect` of values made through
# `a.sample`, so the two instantiations compute different results.
expect encode(make_promoted(Apples.sample())) == Ok(53)
expect encode(make_promoted(Pears.sample())) == Ok(92)
