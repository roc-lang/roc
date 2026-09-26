# Specializes Query's rejecting literal only from code that runs at runtime.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Query
import pf.Stdout

main! = || {
	count = List.len(Str.to_utf8(U64.to_str(Query.plain(1))))
	query : Query.Sql(I32)
	query = Query.get(count)
	Stdout.line!(query.text)
}
