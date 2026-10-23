# A runtime body reaching a literal whose conversion depends on its
# instance, so `roc check` lowers it for discovery, and a second check with
# the same modules parks it through the first check's discovery summary.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import Query
import pf.Stdout

describe : U64 -> Str
describe = |n| {
	first : Query.Query(U64)
	first = Query.make(n)
	second : Query.Query(U64)
	second = Query.make(n + 1)
	Str.concat(Query.render(first), Query.render(second))
}

# Each function is called twice, so none of them is inlined into its caller.
main! = |args| {
	length = args.len().to_u64()
	Stdout.line!(describe(length))
	Stdout.line!(describe(length + 1))
	Ok({})
}
