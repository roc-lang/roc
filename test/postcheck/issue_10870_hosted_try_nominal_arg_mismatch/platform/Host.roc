import OsStr exposing [OsStr]

# Hosted declarations. `var!` hands the app a nominal type, and both functions
# return `Try` at a closed error row, so `?` on either one widens through a
# generated hosted-try adapter.
Host := [].{
	var! : Str => Try(OsStr, [VarNotFound(Str)])
	line! : Str => Try({}, [StdoutErr(Str)])
}
