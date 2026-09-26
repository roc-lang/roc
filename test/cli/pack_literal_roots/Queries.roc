# A custom string literal inside a generic function, reached from closed
# exports at a concrete type. Every build converts the literal at compile
# time, so the exports that reach it are never served from the object cache;
# the plain export is.
Queries :: [].{
	Sql(a) := { text : Str }.{
		from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
		from_quote = |raw| Ok(Sql.{ text: Str.concat(raw, "!") })
	}

	get : U64 -> Sql(a)
	get = |_n| "select 1"

	make : U64 -> Str
	make = |n| {
		query : Sql(I32)
		query = get(n)
		query.text
	}

	plain : U64 -> U64
	plain = |n| n + 1
}
