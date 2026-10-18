# Calls a URL package's closed exports from a wasm32 app. A warm dev build
# composes the package's size-optimized object pack into its object and
# must behave exactly like a build that compiles the package itself.
app [main!] {
	pf: platform "../platform/main.roc",
	num: "https://example.com/roc/numeric/1.0.0/FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ123456789o.tar.zst",
}

import pf.FallibleHost
import num.Numeric

main! = || {
	# The host's input is 21 bytes long, which the compiler cannot know.
	n = Str.count_utf8_bytes(FallibleHost.json_input!({})) - 11
	"${Numeric.describe(n)}\n${Numeric.sum_squares(n + 1).to_str()}"
}
