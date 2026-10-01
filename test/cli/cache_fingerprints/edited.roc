# The edit of main.roc: it also passes Helpers a list that shares its
# allocation with the host's arguments.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Helpers

first : U8 -> U64
first = |n| Helpers.helper([1, 2, n], n) + Helpers.measure([n])

second : U8 -> U64
second = |n| Helpers.helper(Helpers.grow([n], n), n) + Helpers.measure([n, n])

main! = |args| {
	n = args.len().to_u8_wrap()
	bytes = Str.to_utf8(Str.join_with(args, " "))
	Stdout.line!((first(n) + second(n) + Helpers.helper(bytes, n)).to_str())
	Ok({})
}
