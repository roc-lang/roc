import FallibleHost

FallibleReject := [].{
	# Rejected side of the `?` re-raise rule (design.md "Try Question Error
	# Re-raise"): the re-raised row is open, but this annotation omits
	# HostErr(Str) and its rigid extension cannot absorb it, so this is a
	# type error.
	mismatched! : {} => Try(Str, [SomethingElse(Str), ..])
	mismatched! = |{}| Ok(FallibleHost.str_ok!({})?)
}
