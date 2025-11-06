Builtin := [].{
	Str := [ProvidedByCompiler].{
		is_empty : Str -> Bool

		contains : Str, Str -> Bool
		contains = |_str, _other| True
	}

	List := [ProvidedByCompiler].{
		len : List(a) -> U64
		len = |_| 0

		is_empty : List(a) -> Bool
		is_empty = |_| True

		first : List(a) -> Try(a, [ListWasEmpty])
		first = |_| Err(ListWasEmpty)

		map : List(a), (a -> b) -> List(b)
		map = |_, _| []

		keep_if : List(a), (a -> Bool) -> List(a)
		keep_if = |_, _| []

		concat : List(a), List(a) -> List(a)
		concat = |_, _| []
	}

	Bool := [True, False].{
		not : Bool -> Bool
		not = |bool| match bool {
			Bool.True => Bool.False
			Bool.False => Bool.True
		}

		eq : Bool, Bool -> Bool
		eq = |a, b| match a {
			Bool.True => b
			Bool.False => Bool.not(b)
		}

		ne : Bool, Bool -> Bool
		ne = |a, b| match a {
			Bool.True => Bool.not(b)
			Bool.False => b
		}

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

	Set(elem) := [].{
		is_empty : Set(elem) -> Bool

		is_eq : Set(elem), Set(elem) -> Bool
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
		}

		U16 := [].{
			is_zero : U16 -> Bool
			is_eq : U16, U16 -> Bool
			is_gt : U16, U16 -> Bool
			is_gte : U16, U16 -> Bool
			is_lt : U16, U16 -> Bool
			is_lte : U16, U16 -> Bool
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
		}

		U32 := [].{
			is_zero : U32 -> Bool
			is_eq : U32, U32 -> Bool
			is_gt : U32, U32 -> Bool
			is_gte : U32, U32 -> Bool
			is_lt : U32, U32 -> Bool
			is_lte : U32, U32 -> Bool
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
		}

		U64 := [].{
			is_zero : U64 -> Bool
			is_eq : U64, U64 -> Bool
			is_gt : U64, U64 -> Bool
			is_gte : U64, U64 -> Bool
			is_lt : U64, U64 -> Bool
			is_lte : U64, U64 -> Bool
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
		}

		U128 := [].{
			is_zero : U128 -> Bool
			is_eq : U128, U128 -> Bool
			is_gt : U128, U128 -> Bool
			is_gte : U128, U128 -> Bool
			is_lt : U128, U128 -> Bool
			is_lte : U128, U128 -> Bool
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
		}

		Dec := [].{
			is_zero : Dec -> Bool
			is_negative : Dec -> Bool
			is_positive : Dec -> Bool
			is_eq : Dec, Dec -> Bool
			is_gt : Dec, Dec -> Bool
			is_gte : Dec, Dec -> Bool
			is_lt : Dec, Dec -> Bool
			is_lte : Dec, Dec -> Bool
		}

		F32 := [].{
			is_zero : F32 -> Bool
			is_negative : F32 -> Bool
			is_positive : F32 -> Bool
			is_gt : F32, F32 -> Bool
			is_gte : F32, F32 -> Bool
			is_lt : F32, F32 -> Bool
			is_lte : F32, F32 -> Bool
		}

		F64 := [].{
			is_zero : F64 -> Bool
			is_negative : F64 -> Bool
			is_positive : F64 -> Bool
			is_gt : F64, F64 -> Bool
			is_gte : F64, F64 -> Bool
			is_lt : F64, F64 -> Bool
			is_lte : F64, F64 -> Bool
		}
	}
}
