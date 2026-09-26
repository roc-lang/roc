import FallibleHost

FallibleReject := [].{
	# `?` re-raises the hosted callee's error row into this function's
	# return row, and the annotation bounds what this function may produce.
	# FallibleHost.str_ok! can fail with HostErr(Str), which this annotation
	# omits, so this is a type error.
	mismatched! : {} => Try(Str, [SomethingElse(Str)])
	mismatched! = |{}| Ok(FallibleHost.str_ok!({})?)
}
