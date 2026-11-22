app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost
import Parity

main! = || {
	n = 81 + Str.count_utf8_bytes(FallibleHost.json_input!({})) * 0
	Parity.even(n, 0).to_str()
}
