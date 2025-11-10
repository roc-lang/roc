Builtin := [].{
	Str := [ProvidedByCompiler].{
		is_empty : Str -> Bool

		contains : Str, Str -> Bool
		contains = |_str, _other| True

		concat : Str, Str -> Str
	}

	List := [ProvidedByCompiler].{
		len : List(_item) -> U64
		is_empty : List(_item) -> Bool
		concat : List(item), List(item) -> List(item)

		first : List(item) -> Try(item, [ListWasEmpty])
		first = |list| List.get(list, 0)

		get : List(item), U64 -> Try(item, [ListWasEmpty])
		get = |list, index| if index < List.len(list) {
			Try.Ok(list_get_unsafe(list, index))
		} else {
			Try.Err(ListWasEmpty)
		}

		map : List(a), (a -> b) -> List(b)
		map = |_, _| []

		keep_if : List(a), (a -> Bool) -> List(a)
		keep_if = |_, _| []

		fold : List(item), state, (state, item -> state) -> state
		fold = |list, init, step| {
			var $state = init

			for item in list {
				$state = step($state, item)
			}

			$state
		}
	}

	Bool := [True, False].{
		not : Bool -> Bool
		not = |bool| match bool {
			Bool.True => Bool.False
			Bool.False => Bool.True
		}

		is_eq : Bool, Bool -> Bool
		is_ne : Bool, Bool -> Bool

		#encoder : Bool -> Encoder(fmt, [])
		#	where [fmt implements EncoderFormatting]
		#encoder =

		#Encoder fmt := List U8, fmt -> List U8 where fmt implements EncoderFormatting
	}

	Try(ok, err) := [Ok(ok), Err(err)].{
		is_ok : Try(_ok, _err) -> Bool
		is_ok = |res| match res {
			Ok(_) => True
			Err(_) => False
		}

		is_err : Try(_ok, _err) -> Bool
		is_err = |res| match res {
			Ok(_) => False
			Err(_) => True
		}

		#eq : Try(ok, err), Try(ok, err) -> Bool
		#	where [
		#		ok.equals : ok, ok -> Bool,
		#		err.equals : ok, ok -> Bool,
		#	]
		#eq = |a, b| match a {
		#	Ok(a_val) => {
		#		match b {
		#			Ok(b_val) => a_val.equals(b_val)
		#			Err(_) => False
		#		}
		#	}
		#	Err(a_val) => {
		#		match b {
		#			Ok(_) => False
		#			Err(b_val) => a_val.equals(b_val)
		#		}
		#	}
		#}
	}

	Dict := [EmptyDict].{}

	Set(item) := [].{
		is_empty : Set(item) -> Bool

		is_eq : Set(item), Set(item) -> Bool
		is_eq = |_a, _b| Bool.False
	}

	Num := {}.{
		U8 := [].{
			is_zero : U8 -> Bool
			is_eq : U8, U8 -> Bool
			is_gt : U8, U8 -> Bool
			is_gte : U8, U8 -> Bool
			is_lt : U8, U8 -> Bool
			is_lte : U8, U8 -> Bool

			plus : U8, U8 -> U8
			minus : U8, U8 -> U8
			times : U8, U8 -> U8
			div_by : U8, U8 -> U8
			rem_by : U8, U8 -> U8

			from_int_digits : List(U8) -> Try(U8, [OutOfRange])
		}

		I8 := [].{
			is_zero : I8 -> Bool
			is_negative : I8 -> Bool
			is_positive : I8 -> Bool
			is_eq : I8, I8 -> Bool
			is_gt : I8, I8 -> Bool
			is_gte : I8, I8 -> Bool
			is_lt : I8, I8 -> Bool
			is_lte : I8, I8 -> Bool

			negate : I8 -> I8
			plus : I8, I8 -> I8
			minus : I8, I8 -> I8
			times : I8, I8 -> I8
			div_by : I8, I8 -> I8
			rem_by : I8, I8 -> I8

			from_int_digits : List(U8) -> Try(I8, [OutOfRange])
		}

		U16 := [].{
			is_zero : U16 -> Bool
			is_eq : U16, U16 -> Bool
			is_gt : U16, U16 -> Bool
			is_gte : U16, U16 -> Bool
			is_lt : U16, U16 -> Bool
			is_lte : U16, U16 -> Bool

			plus : U16, U16 -> U16
			minus : U16, U16 -> U16
			times : U16, U16 -> U16
			div_by : U16, U16 -> U16
			rem_by : U16, U16 -> U16

			from_int_digits : List(U8) -> Try(U16, [OutOfRange])
		}

		I16 := [].{
			is_zero : I16 -> Bool
			is_negative : I16 -> Bool
			is_positive : I16 -> Bool
			is_eq : I16, I16 -> Bool
			is_gt : I16, I16 -> Bool
			is_gte : I16, I16 -> Bool
			is_lt : I16, I16 -> Bool
			is_lte : I16, I16 -> Bool

			negate : I16 -> I16
			plus : I16, I16 -> I16
			minus : I16, I16 -> I16
			times : I16, I16 -> I16
			div_by : I16, I16 -> I16
			rem_by : I16, I16 -> I16

			from_int_digits : List(U8) -> Try(I16, [OutOfRange])
		}

		U32 := [].{
			is_zero : U32 -> Bool
			is_eq : U32, U32 -> Bool
			is_gt : U32, U32 -> Bool
			is_gte : U32, U32 -> Bool
			is_lt : U32, U32 -> Bool
			is_lte : U32, U32 -> Bool

			plus : U32, U32 -> U32
			minus : U32, U32 -> U32
			times : U32, U32 -> U32
			div_by : U32, U32 -> U32
			rem_by : U32, U32 -> U32

			from_int_digits : List(U8) -> Try(U32, [OutOfRange])
		}

		I32 := [].{
			is_zero : I32 -> Bool
			is_negative : I32 -> Bool
			is_positive : I32 -> Bool
			is_eq : I32, I32 -> Bool
			is_gt : I32, I32 -> Bool
			is_gte : I32, I32 -> Bool
			is_lt : I32, I32 -> Bool
			is_lte : I32, I32 -> Bool

			negate : I32 -> I32
			plus : I32, I32 -> I32
			minus : I32, I32 -> I32
			times : I32, I32 -> I32
			div_by : I32, I32 -> I32
			rem_by : I32, I32 -> I32

			from_int_digits : List(U8) -> Try(I32, [OutOfRange])
		}

		U64 := [].{
			is_zero : U64 -> Bool
			is_eq : U64, U64 -> Bool
			is_gt : U64, U64 -> Bool
			is_gte : U64, U64 -> Bool
			is_lt : U64, U64 -> Bool
			is_lte : U64, U64 -> Bool

			plus : U64, U64 -> U64
			minus : U64, U64 -> U64
			times : U64, U64 -> U64
			div_by : U64, U64 -> U64
			rem_by : U64, U64 -> U64

			from_int_digits : List(U8) -> Try(U64, [OutOfRange])
		}

		I64 := [].{
			is_zero : I64 -> Bool
			is_negative : I64 -> Bool
			is_positive : I64 -> Bool
			is_eq : I64, I64 -> Bool
			is_gt : I64, I64 -> Bool
			is_gte : I64, I64 -> Bool
			is_lt : I64, I64 -> Bool
			is_lte : I64, I64 -> Bool

			negate : I64 -> I64
			plus : I64, I64 -> I64
			minus : I64, I64 -> I64
			times : I64, I64 -> I64
			div_by : I64, I64 -> I64
			rem_by : I64, I64 -> I64

			from_int_digits : List(U8) -> Try(I64, [OutOfRange])
		}

		U128 := [].{
			is_zero : U128 -> Bool
			is_eq : U128, U128 -> Bool
			is_gt : U128, U128 -> Bool
			is_gte : U128, U128 -> Bool
			is_lt : U128, U128 -> Bool
			is_lte : U128, U128 -> Bool

			plus : U128, U128 -> U128
			minus : U128, U128 -> U128
			times : U128, U128 -> U128
			div_by : U128, U128 -> U128
			rem_by : U128, U128 -> U128

			from_int_digits : List(U8) -> Try(U128, [OutOfRange])
		}

		I128 := [].{
			is_zero : I128 -> Bool
			is_negative : I128 -> Bool
			is_positive : I128 -> Bool
			is_eq : I128, I128 -> Bool
			is_gt : I128, I128 -> Bool
			is_gte : I128, I128 -> Bool
			is_lt : I128, I128 -> Bool
			is_lte : I128, I128 -> Bool

			negate : I128 -> I128
			plus : I128, I128 -> I128
			minus : I128, I128 -> I128
			times : I128, I128 -> I128
			div_by : I128, I128 -> I128
			rem_by : I128, I128 -> I128

			from_int_digits : List(U8) -> Try(I128, [OutOfRange])
		}

		Dec := [].{
			is_zero : Dec -> Bool
			is_negative : Dec -> Bool
			is_positive : Dec -> Bool
			is_eq : Dec, Dec -> Bool
			is_ne : Dec, Dec -> Bool
			is_gt : Dec, Dec -> Bool
			is_gte : Dec, Dec -> Bool
			is_lt : Dec, Dec -> Bool
			is_lte : Dec, Dec -> Bool

			negate : Dec -> Dec
			plus : Dec, Dec -> Dec
			minus : Dec, Dec -> Dec
			times : Dec, Dec -> Dec
			div_by : Dec, Dec -> Dec
			rem_by : Dec, Dec -> Dec

			from_int_digits : List(U8) -> Try(Dec, [OutOfRange])
			from_dec_digits : (List(U8), List(U8)) -> Try(Dec, [OutOfRange])
		}

		F32 := [].{
			is_zero : F32 -> Bool
			is_negative : F32 -> Bool
			is_positive : F32 -> Bool
			is_gt : F32, F32 -> Bool
			is_gte : F32, F32 -> Bool
			is_lt : F32, F32 -> Bool
			is_lte : F32, F32 -> Bool

			negate : F32 -> F32
			plus : F32, F32 -> F32
			minus : F32, F32 -> F32
			times : F32, F32 -> F32
			div_by : F32, F32 -> F32
			rem_by : F32, F32 -> F32

			from_int_digits : List(U8) -> Try(F32, [OutOfRange])
			from_dec_digits : (List(U8), List(U8)) -> Try(F32, [OutOfRange])
		}

		F64 := [].{
			is_zero : F64 -> Bool
			is_negative : F64 -> Bool
			is_positive : F64 -> Bool
			is_gt : F64, F64 -> Bool
			is_gte : F64, F64 -> Bool
			is_lt : F64, F64 -> Bool
			is_lte : F64, F64 -> Bool

			negate : F64 -> F64
			plus : F64, F64 -> F64
			minus : F64, F64 -> F64
			times : F64, F64 -> F64
			div_by : F64, F64 -> F64
			rem_by : F64, F64 -> F64

			from_int_digits : List(U8) -> Try(F64, [OutOfRange])
			from_dec_digits : (List(U8), List(U8)) -> Try(F64, [OutOfRange])
		}
	}
}

# Private top-level function for unsafe list access
# This is a low-level operation that gets replaced by the compiler
list_get_unsafe : List(item), U64 -> item
