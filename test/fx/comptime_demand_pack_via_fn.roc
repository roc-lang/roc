app [main!] {
	lib: "./comptime_demand_pack_pkg/main.roc",
	pf: platform "./platform/main.roc",
}

import pf.Stdout
import lib.Lib

main! = || {
	Stdout.line!("b: ${Lib.sum_table(10).to_str()}")
}
