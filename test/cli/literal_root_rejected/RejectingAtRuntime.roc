# Specializes Query's rejecting literal only from code that depends on stdin,
# so no compile-time root reads it.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Query
import pf.Stdin
import pf.Stdout

main! = || {
	query : Query.Sql(I32)
	query = Query.get(Str.count_utf8_bytes(Stdin.line!()))
	Stdout.line!(query.text)
}
