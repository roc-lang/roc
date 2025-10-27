Builtin := [].{
	Str := [ProvidedByCompiler].{
		is_empty : Str -> Bool

		contains : Str, Str -> Bool
		contains = |_str, _other| True
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

	Result(ok, err) := [Ok(ok), Err(err)].{
		is_ok : Result(_ok, _err) -> Bool
		is_ok = |res| match res {
			Ok(_) => True
			Err(_) => False
		}

		is_err : Result(_ok, _err) -> Bool
		is_err = |res| match res {
			Ok(_) => False
			Err(_) => True
		}

		#eq : Result(ok, err), Result(ok, err) -> Bool
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

	Set := [EmptySet(Dict)].{}
}
