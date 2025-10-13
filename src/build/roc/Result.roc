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

	#equals : Result(ok, err), Result(ok, err) -> Bool
	#	where [
	#		ok.equals : ok, ok -> Bool,
	#		err.equals : ok, ok -> Bool,
	#	]
	#equals = |a, b| match a {
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
