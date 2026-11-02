# A dev build whose erased callables reach the Boxy runtime, and whose `grow`
# builtin code calls as well as the app's own loop. Its dev packs serve the
# second build the code the first one compiled, and no build may differ from
# one without the object cache: the runtime's layout tables are numbered by
# what the image's code uses, and every build gives the app's functions the
# same signatures and variants.
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

grow : List(U64), U64 -> Try(List(U64), [TooLong])
grow = |list, item| if list.len() > 1000 {
	Err(TooLong)
} else {
	Ok(list.append(item))
}

grown : U64 -> U64
grown = |n| {
	var $list = []
	var $index = 0
	while $index < n {
		$list = match grow($list, $index) {
			Ok(next) => next
			Err(TooLong) => $list
		}
		$index = $index + 1
	}
	$list.len()
}

main! = |args| {
	n = List.len(args) + 9
	fs = [boxed, Box.box(|x| x + ink), Shapes.scale(n)]
	total = fs.map(|f| Box.unbox(f)(n)).sum()
	Stdout.line!(Numeric.describe(n))
	Stdout.line!(Shapes.label(total))
	folded = [1, 2, 3].fold_try([], grow) ?? []
	Stdout.line!("${folded.len().to_str()} ${grown(n).to_str()}")
	Ok({})
}
