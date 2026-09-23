app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

emit_total! = || {
	bound = Str.count_utf8_bytes(Stdin.line!())
	total = (0.U64).until(bound).map(|n| n * 2).fold(0, |sum, n| sum + n)
	Stdout.line!(Str.inspect(total))
}

main! = || {
	emit_total!()
	emit_total!()
	emit_total!()
}
