import FallibleHost

FallibleReject := [].{
	# Rejected side of the hosted-try-question-widening rule (design.md
	# "Hosted Try Question Widening"): `?` on a direct hosted call widens only
	# when every visible error in the hosted callee's row is included in the
	# enclosing annotated return row. FallibleHost.str_ok! can fail with
	# HostErr(Str), which this annotation omits, so this is a type error.
	mismatched! : {} => Try(Str, [SomethingElse(Str), ..])
	mismatched! = |{}| Ok(FallibleHost.str_ok!({})?)
}
