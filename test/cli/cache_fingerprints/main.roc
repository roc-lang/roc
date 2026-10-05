# Built, then replaced by edited.roc and built again into the same object
# cache, so the second build links procedures compiled for this one.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Helpers

first : U8 -> U64
first = |n| Helpers.helper([1, 2, n], n) + Helpers.outer(n.to_u64())

second : U8 -> U64
second = |n| Helpers.helper(Helpers.grow([n], n), n) + Helpers.outer(n.to_u64() + 1)

main! = |args| {
	n = args.len().to_u8_wrap()
	widths = Helpers.widths([Str.to_utf8("ab"), Str.to_utf8("c")])
	Stdout.line!((first(n) + second(n) + widths.len()).to_str())
	Ok({})
}
