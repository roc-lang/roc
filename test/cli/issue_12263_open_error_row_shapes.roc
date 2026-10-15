# repro for https://github.com/roc-lang/roc/issues/12263
# Each call below reaches `transaction`, whose callback row `[..errs]` is
# extended by `Closed` in its result, with a callback whose own errors include
# `Closed`: through `?` on another method, through a `where` requirement used
# twice, and through exact procedure aliases called as methods and qualified.
Shapes := [Db].{
	transaction : Shapes, (() -> Try(a, [..errs])) -> Try(a, [Closed, ..errs])
	transaction = |_db, body| Ok(body()?)
	begin : Shapes, Bool -> Try({}, [Closed])
	begin = |_db, closed| if closed { Err(Closed) } else { Ok({}) }
	tx = transaction
	tx_again = tx
	narrow : Shapes, (() -> Try(Str, [Closed, ..e])) -> Try(Str, [Closed, ..e])
	narrow = transaction
}

withdraw : Shapes, Bool, U64 -> Try(U64, [Closed, Insufficient])
withdraw = |db, closed, amount|
	db.transaction(|| {
		db.begin(closed)?
		if amount > 100 { Err(Insufficient) } else { Ok(amount) }
	})

expect withdraw(Shapes.Db, Bool.False, 5) == Ok(5)
expect withdraw(Shapes.Db, Bool.True, 5) == Err(Closed)
expect withdraw(Shapes.Db, Bool.False, 500) == Err(Insufficient)

through_where : d, Bool -> Try(Str, [Closed]) where [d.transaction : d, (() -> Try(Str, [Closed])) -> Try(Str, [Closed])]
through_where = |db, closed| {
	first = db.transaction(|| if closed { Err(Closed) } else { Ok("done") })
	second = db.transaction(|| Ok("again"))
	match (first, second) {
		(Ok(a), Ok(b)) => Ok(Str.concat(a, b))
		(Err(e), _) => Err(e)
		(_, Err(e)) => Err(e)
	}
}

expect through_where(Shapes.Db, Bool.False) == Ok("doneagain")
expect through_where(Shapes.Db, Bool.True) == Err(Closed)

closing : Bool -> (() -> Try(Str, [Closed]))
closing = |closed| || if closed { Err(Closed) } else { Ok("done") }

expect Shapes.Db.tx(closing(Bool.False)) == Ok("done")
expect Shapes.Db.tx(closing(Bool.True)) == Err(Closed)
expect Shapes.tx(Shapes.Db, closing(Bool.True)) == Err(Closed)
expect Shapes.Db.tx_again(closing(Bool.True)) == Err(Closed)
expect Shapes.tx_again(Shapes.Db, closing(Bool.False)) == Ok("done")
expect Shapes.Db.narrow(closing(Bool.True)) == Err(Closed)
failing : Bool -> (() -> Try(U64, [Other]))
failing = |fail| || if fail { Err(Other) } else { Ok(1) }

expect Shapes.Db.tx(failing(Bool.True)) == Err(Other)
