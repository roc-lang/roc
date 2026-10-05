# The edit of main.roc: it also passes Helpers a list that shares its
# allocation with the host's arguments, calls Helpers.other, so a
# whole-program count would see two calls to Helpers' `inner`, and imports
# Extra.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Helpers
import Extra

first : U8 -> U64
first = |n| Helpers.helper([1, 2, n], n) + Helpers.outer(n.to_u64())

second : U8 -> U64
second = |n| Helpers.helper(Helpers.grow([n], n), n) + Helpers.outer(n.to_u64() + 1)

main! = |args| {
	n = args.len().to_u8_wrap()
	bytes = Str.to_utf8(Str.join_with(args, " "))
	widths = Helpers.widths([bytes, Str.to_utf8("c")])
	Stdout.line!((first(n) + second(n) + Helpers.helper(bytes, n) + Helpers.other(n.to_u64()) + Extra.total(bytes) + widths.len()).to_str())
	Ok({})
}
