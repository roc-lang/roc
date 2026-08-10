# repro for https://github.com/roc-lang/roc/issues/10689
# `r` is annotated with an open error union that does not accept the open error
# union `fetch` returns, so `roc build` must report that type mismatch.
app [main!] { pf: platform "../fx-open/platform/main.roc" }
import pf.Stdout

fetch : Str -> Try(Str, [Missing, ..])
fetch = |s| Ok(s)

main! = |_args| {
	r : Try(Str, [Bad, ..])
	r = fetch("a")
	match r {
		Ok(v) => Stdout.line!(v)
		Err(_) => Stdout.line!("err")
	}
	Ok({})
}
