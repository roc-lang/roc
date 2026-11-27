app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost
import Loop

main! = || {
	n = 5 + Str.count_utf8_bytes(FallibleHost.json_input!({})) * 0
	Loop.ping({ a: 1, b: "r" }, n, ">")
}
