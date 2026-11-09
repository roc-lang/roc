# repro for https://github.com/roc-lang/roc/issues/12263
# A method and a method alias imported from another module, called with a
# callback whose errors include the tag the method adds to its result row.
import Store

User := [U].{
	direct : Store, Bool -> Try(Str, [Closed])
	direct = |db, closed| db.transaction(|| if closed { Err(Closed) } else { Ok("a") })
	aliased : Store, Bool -> Try(Str, [Closed])
	aliased = |db, closed| db.tx(|| if closed { Err(Closed) } else { Ok("b") })
}

expect User.direct(Store.Db, Bool.False) == Ok("a")
expect User.direct(Store.Db, Bool.True) == Err(Closed)
expect User.aliased(Store.Db, Bool.False) == Ok("b")
expect User.aliased(Store.Db, Bool.True) == Err(Closed)
