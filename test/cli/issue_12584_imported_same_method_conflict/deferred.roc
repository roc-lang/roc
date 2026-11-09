app [main!] { pf: platform "../../fx-open/platform/main.roc" }

# repro for https://github.com/roc-lang/roc/issues/12584
# The `get` conflict in DeferredJob.roc is found only when an obligation the
# merged requirement deferred fails, after `run` already used `both`.

import DeferredJob

P := { s : Str }.{
	from_quote : Str -> Try(P, [BadQuotedBytes(Str)])
	from_quote = |s| Ok(P.{ s })
}

Db := {}.{
	get : Db, (U64 -> a) -> Try(a, [Failed(_)])
	get = |_db, _f| Err(Failed("x"))
}

main! = |_args| {
	_ = DeferredJob.run({ db: Db.{}, p: P.{ s: "x" } })
	Ok({})
}
