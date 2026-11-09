# repro for https://github.com/roc-lang/roc/issues/12263
# A method call whose callback has error row `[..errs]` and whose result has
# `[Closed, ..errs]`, where the callback's own errors include `Closed`, must
# lower and run exactly like the equivalent qualified call.
Method := [Db].{
	transaction : Method, (() -> Try(a, [..errs])) -> Try(a, [Closed, ..errs])
	transaction = |_db, body| Ok(body()?)
}

run : Method, Bool -> Try(Str, [Closed])
run = |db, closed|
	db.transaction(|| if closed { Err(Closed) } else { Ok("done") })

expect run(Method.Db, Bool.False) == Ok("done")
expect run(Method.Db, Bool.True) == Err(Closed)
