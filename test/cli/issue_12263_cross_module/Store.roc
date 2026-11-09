Store := [Db].{
	transaction : Store, (() -> Try(a, [..errs])) -> Try(a, [Closed, ..errs])
	transaction = |_db, body| Ok(body()?)
	tx = transaction
}
