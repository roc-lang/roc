app [main!] {
	lib: "./comptime_demand_pack_pkg/main.roc",
	pf: platform "./platform/main.roc",
}

import pf.Stdout
import lib.Lib

main! = || {
	Stdout.line!("a: ${Lib.double(21).to_str()}")
}
