app [main!] {
	lib: "./comptime_demand_pack_pkg/main.roc",
	pf: platform "./platform/main.roc",
}

import pf.Stdout
import lib.Lib

main! = || {
	Stdout.line!("c: ${Lib.double(List.len(Lib.table)).to_str()}")
}
