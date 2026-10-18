# Calls a URL package's closed exports from a wasm32 app. A warm dev build
# composes the package's size-optimized object pack into its object and
# must behave exactly like a build that compiles the package itself.
app [main!] {
	pf: platform "../static-lib-platform/main.roc",
	num: "https://example.com/roc/numeric/1.0.0/FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ123456789o.tar.zst",
}

import num.Numeric

main! = |seed| {
	# The host's seed is 0, which the compiler cannot know.
	n = seed + 10
	# 10 shifted left four times sets every lane's high bit.
	shifted = Numeric.shift_lanes(U8x16.splat(n.to_u8_wrap()), seed + 4)
	"${Numeric.describe(n)}\n${Numeric.sum_squares(n + 1).to_str()}\n${shifted.to_bitmask().to_str()}"
}
