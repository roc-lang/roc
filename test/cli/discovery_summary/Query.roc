# A literal type whose conversion rejects a keyword. `make`'s literal is
# converted at the instance its caller picks, so checking discovers it from
# the runtime program rather than evaluating it as a compile-time root.
Query(a) := { text : Str }.{
	from_quote : Str -> Try(Query(a), [BadQuotedBytes(Str)])
	from_quote = |raw| if raw.contains("DROP") Err(BadQuotedBytes("rejected query")) else Ok({ text: raw })

	make : U64 -> Query(a)
	make = |_n| "SELECT 1"

	render : Query(a) -> Str
	render = |query| query.text
}
