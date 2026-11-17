app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

# Regression test for issue 13029: a wasm32 module's static data constants
# are merged into one standard data segment rather than each getting a
# segment of its own, and a constant nothing reachable references is not in
# the module.
#
# `labels` gives the program six constants, each a string too long to store
# inline, which all lie in one data segment. `unused_label` is named only by a branch whose condition, a 32-bit
# count compared against a larger 64-bit bound, is always false; the
# optimizer removes the branch, so nothing references the constant.
labels : List(Str)
labels = [
	"the first label is too long to be stored inline",
	"the second label is too long to be stored inline",
	"the third label is too long to be stored inline",
	"the fourth label is too long to be stored inline",
	"the fifth label is too long to be stored inline",
	"the sixth label is too long to be stored inline",
]

unused_label : Str
unused_label = "UNREACHABLE-STATIC-CONSTANT-MARKER-0123456789"

main! : () => Str
main! = || {
	n = FallibleHost.json_input!({}).count_utf8_bytes()
	label = match labels.get(n % 6) {
		Ok(text) => text
		Err(_) => ""
	}
	if n.to_u32_wrap().to_u64() > 5000000000 unused_label else label
}
