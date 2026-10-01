# Built, then replaced by edited.roc and built again into the same object
# cache, so the second build links Helpers procedures compiled for this one.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Helpers

first : U8 -> U64
first = |n| Helpers.helper([1, 2, n], n) + Helpers.measure([n])

second : U8 -> U64
second = |n| Helpers.helper(Helpers.grow([n], n), n) + Helpers.measure([n, n])

main! = |args| {
	n = args.len().to_u8_wrap()
	Stdout.line!((first(n) + second(n)).to_str())
	Ok({})
}
