# Calls a URL package's closed exports. A warm dev build links the
# package's LLVM-optimized object pack and must behave exactly like a build
# that compiles the package itself.
app [main!] {
	pf: platform "../../fx-open/platform/main.roc",
	num: "https://example.com/roc/numeric/1.0.0/FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ123456789o.tar.zst",
}

import pf.Stdout
import num.Numeric

main! = |args| {
	n = List.len(args) + 9
	Stdout.line!(Numeric.describe(n))
	Stdout.line!(Numeric.sum_squares(n + 1).to_str())
	Stdout.line!(Numeric.weighted(n).to_str())
	Ok({})
}
