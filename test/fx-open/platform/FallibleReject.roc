import FallibleHost

FallibleReject := [].{
	# A hosted error omitted from the enclosing return row remains a type error.
	mismatched! : {} => Try(Str, [SomethingElse(Str)])
	mismatched! = |{}| Ok(FallibleHost.str_ok!({})?)
}
