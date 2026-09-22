app [main!] { pf: platform "./platform/main.roc" }

# Regression test for https://github.com/roc-lang/roc/issues/11097
#
# A bare `?` and a tag-wrapped `?` on the same monomorphic callee compose the
# row `[PersistFailed(e), ..e]`. At runtime the bare `?` returns the callee's
# narrower error across the return boundary into that wider row, and the body's
# own `Ok` crosses it too, so each path must come out under its own tag.

import pf.Stdout

run = |save| {
	_ = save("a")?
	_ = save("b") ? PersistFailed
	Ok({})
}

fail_on : Str -> (Str -> Try({}, [DbErr(Str)]))
fail_on = |bad| |key| if key == bad { Err(DbErr(key)) } else { Ok({}) }

describe : Try({}, [PersistFailed([DbErr(Str)]), DbErr(Str)]) -> Str
describe = |result|
	match result {
		Ok({}) => "ok"
		Err(DbErr(k)) => "DbErr(${k})"
		Err(PersistFailed(DbErr(k))) => "PersistFailed(DbErr(${k}))"
	}

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
	Stdout.line!("bare path:    ${describe(run(fail_on("a")))}")
	Stdout.line!("wrapped path: ${describe(run(fail_on("b")))}")
	Stdout.line!("ok path:      ${describe(run(fail_on("z")))}")
	Ok({})
}
