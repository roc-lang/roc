app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout
import pf.Stdin

# The returned closure is generic in its argument's item type, so its worker
# reads that list with erased items. Calling it with a `List(I64)` converts the
# argument into that form, and the call owns its argument, so the conversion
# consumes the original list.
make : U64 -> Try((List(item) -> U64), [TooBig])
make = |n| if n > 100 { Err(TooBig) } else { Ok(|values| List.len(values) + n) }

main! = || {
	n = Str.count_utf8_bytes(Stdin.line!())
	total = match make(n) {
		Ok(f) => f([1.I64, 2])
		Err(_) => 0
	}
	Stdout.line!(Str.inspect(total))
}
