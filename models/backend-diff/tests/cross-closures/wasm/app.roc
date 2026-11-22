app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost
import Base
import Mid

main! = || {
	zu = Str.count_utf8_bytes(FallibleHost.json_input!({})) * 0
	k = zu.to_i64_wrap() + 4
	r = Mid.run([1, 2, 3], k)
	t = match Base.shape(r.a) {
		P(n) => Str.concat("P", n.to_str())
		Q(s) => Str.concat("Q", s)
		W => "W"
	}
	lines = [r.a.to_str(), r.b, t, Mid.sum_to(10 + zu, 0).to_str()]
	Str.join_with(lines, "\n")
}
