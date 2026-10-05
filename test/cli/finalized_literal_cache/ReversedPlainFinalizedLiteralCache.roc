app [main!] { pf: platform "../../fx/platform/main.roc" }

import PlainValues
import pf.Stdout

# The actual types remain the same; only caller activation order changes.
main! = || {
	count = List.len(PlainValues.make_u64(3).to_utf8())
	Stdout.line!(PlainValues.make_u64(count))
	Stdout.line!(PlainValues.make_i32(count))
	Stdout.line!(U64.to_str(PlainValues.plain(count)))
}
