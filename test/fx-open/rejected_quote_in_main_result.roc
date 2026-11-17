app [main!] { pf: platform "./platform/main.roc" }

# The rejected literal reaches only the value `main!` returns: its type
# `Sql(a)` names `main!`'s own variable, so the platform's call of `main!`
# is the instantiation that converts it.
Sql(a) := { text : Str }.{
	from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
	from_quote = |raw|
		if raw == "bad" {
			Err(BadQuotedBytes("rejected in main's result"))
		} else {
			Ok(Sql.{ text: raw })
		}
}

wrap : Sql(a) -> Sql(a)
wrap = |sql| sql

main! = |args| {
	if args.len() > 5 {
		return Err(Bad(wrap("bad")))
	}
	Ok({})
}
