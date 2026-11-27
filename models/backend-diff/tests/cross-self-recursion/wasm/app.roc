app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost
import M0

main! = || {
	_zu = Str.count_utf8_bytes(FallibleHost.json_input!({})) * 0
	f = M0.nt(2, "a")
	f(3).to_str()
}
