# A generic function whose custom string literal is rejected at every type,
# which no specialization inside this module makes.
Query :: [].{
	Sql(a) := { text : Str }.{
		from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
		from_quote = |_| Err(BadQuotedBytes("rejected"))
	}

	get : U64 -> Sql(a)
	get = |_n| "select 1"

	plain : U64 -> U64
	plain = |n| n + 1
}
