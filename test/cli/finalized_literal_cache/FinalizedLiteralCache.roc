# Runtime calls reach both exact instantiations. App-only edits must not
# invalidate unchanged imported converter/source/type/evidence identities.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Values
import pf.Stdout

main! = || {
	count = List.len(Values.make_i32(3).to_utf8())
	Stdout.line!(Values.make_i32(count))
	Stdout.line!(Values.make_u64(count))
}
