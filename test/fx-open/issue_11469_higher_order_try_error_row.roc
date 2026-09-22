app [main!] { pf: platform "./platform/main.roc" }

# Regression test for https://github.com/roc-lang/roc/issues/11469
#
# `transaction` wraps one `?` error and returns `operation`'s `Try` in tail
# position, so its error row INCLUDES `operation`'s rather than equalling it.
# A caller may therefore pass a callback whose errors come from the very
# `execute` that `transaction` also wraps. At runtime the callback's narrower
# `Try` crosses the return boundary into `transaction`'s wider row, so each
# error must come out under its own tag: `BeginFailed` only from the wrapped
# call, a bare `DbErr` only from the callback.

import pf.Stdout

transaction = |execute, operation| {
	_ = execute("BEGIN") ? BeginFailed
	operation({})
}

save = |execute|
	transaction(execute, |{}| {
		_ = execute("INSERT")?
		Ok({})
	})

forwarding_save = |execute| transaction(execute, |{}| execute("INSERT"))

fail_on : Str -> (Str -> Try({}, [DbErr(Str)]))
fail_on = |bad| |sql| if sql == bad { Err(DbErr(sql)) } else { Ok({}) }

describe : Try({}, [BeginFailed([DbErr(Str)]), DbErr(Str)]) -> Str
describe = |result|
	match result {
		Ok({}) => "ok"
		Err(DbErr(sql)) => "DbErr(${sql})"
		Err(BeginFailed(DbErr(sql))) => "BeginFailed(DbErr(${sql}))"
	}

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
	Stdout.line!("save begin fails: ${describe(save(fail_on("BEGIN")))}")
	Stdout.line!("save insert fails: ${describe(save(fail_on("INSERT")))}")
	Stdout.line!("save ok: ${describe(save(fail_on("NONE")))}")
	Stdout.line!("forward begin fails: ${describe(forwarding_save(fail_on("BEGIN")))}")
	Stdout.line!("forward insert fails: ${describe(forwarding_save(fail_on("INSERT")))}")
	Stdout.line!("forward ok: ${describe(forwarding_save(fail_on("NONE")))}")
	Ok({})
}
