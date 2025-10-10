import Bool

Result(ok, err) := [Ok(ok), Err(err)].{
	## Returns [True] if the [Result] is [Ok], and [False] if it's [Err].
	##
	## ```roc
	## Ok(5).is_ok()
	## ```
	is_ok : Result(_ok, _err) -> Bool
	is_ok = |res| match res {
		Ok(_) => True
		Err(_) => False
	}

	## Returns [True] if the [Result] is [Err], and [False] if it's [Ok].
	##
	## ```roc
	## Err("error message").is_err()
	## ```
	is_err : Result(_ok, _err) -> Bool
	is_err = |res| match res {
		Ok(_) => False
		Err(_) => True
	}
}
