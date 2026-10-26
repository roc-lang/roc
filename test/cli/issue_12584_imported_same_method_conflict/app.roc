app [main!] { pf: platform "../../fx-open/platform/main.roc" }

# repro for https://github.com/roc-lang/roc/issues/12584
# `roc check` must report the conflicting `query` methods required in Job.roc
# rather than crashing in post-check monotype lowering.

import pf.Stdout
import Job

Db := {}.{
	query : Db, (Bool -> a) -> List(a)
	query = |_db, decode| [decode(True)]
}

run_one = |f| f(Db.{})

main! = |_args| {
	run_one(Job.run)
	Stdout.line!("hi")
	Ok({})
}
