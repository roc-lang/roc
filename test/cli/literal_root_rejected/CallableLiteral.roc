# A custom literal whose type holds a callable. Checking leaves such a
# conversion to the program, which still converts it at compile time and
# reports the rejection.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdout

Sql := { text : Str, render : Str -> Str }.{
	from_quote : Str -> Try(Sql, [BadQuotedBytes(Str)])
	from_quote = |_| Err(BadQuotedBytes("rejected"))
}

main! = || {
	query : Sql
	query = "select 1"
	Stdout.line!((query.render)(query.text))
}
