# A dev build whose erased callables reach the Boxy runtime. Its dev packs
# serve the second build the code the first one compiled, and the executable
# must not change: the runtime's layout tables are numbered by what the
# image's code uses, never by what this build happened to lower.
app [main!] {
	num: "https://example.com/roc/numeric/1.0.0/FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ123456789o.tar.zst",
	pf: platform "../../fx-open/platform/main.roc",
}

import pf.Stdout
import num.Numeric
import Shapes

base = 6.U64

ink = base + 1

boxed : Box(U64 -> U64)
boxed = Box.box(|x| x)

main! = |args| {
	n = List.len(args) + 9
	fs = [boxed, Box.box(|x| x + ink), Shapes.scale(n)]
	total = fs.map(|f| Box.unbox(f)(n)).sum()
	Stdout.line!(Numeric.describe(n))
	Stdout.line!(Shapes.label(total))
	Ok({})
}
