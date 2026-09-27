app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11723
import pf.Stdout

Sql(a) := { text : Str }.{
	from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
	from_quote = |raw| Ok(Sql.{ text: raw })
}

Db(a) := { name : Str }.{
	execute : Db(a), Sql(a) -> Try({}, [Nope])
	execute = |_db, _sql| Ok({})

	query : Db(a), Sql(a), params -> Try({}, [Nope])
	query = |_db, _sql, _params| Err(Nope)
}

typed : Db(Str) -> Try({}, [Nope])
typed = |db| db.execute("good")

f = |db| db.query("good", {})

# `f` always fails, so optimized builds prove the `typed` call and its
# compile-time literal read dead after `?`.
run : Db(Str) -> Try({}, [Nope])
run = |db| {
	_ = f(db)?
	_ = typed(db)
	Ok({})
}

main! = |_args| {
	db : Db(Str)
	db = Db.{ name: "x" }
	_ = run(db)
	Stdout.line!("done")
	Ok({})
}
