# repro for https://github.com/roc-lang/roc/issues/12189
# An imported function over a nominal wrapping a generic tuple must return
# the computed element, so the first element is 10 + 5.
import Issue12189Mod

main! = |_args| {
	res = Issue12189Mod.step({ a: (10.U64, 20.U64) }, 5.U64)
	echo!("${res.a.0.to_str()}\n")
	Ok({})
}
