# A transparent alias for the same closed ABI as FallibleHost.str_ok!.
# Roc wrappers reconstruct the error after receiving this declared result.
IoResult(a) : Try(a, [HostErr(Str)])

FallibleHostAlias := [].{
	str_ok! : {} => IoResult(Str)
}
