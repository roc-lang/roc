app [main!] {
	pf: platform "./platform/main.roc",
	tables: "./comptime_demand_pkg/main.roc",
}

import pf.Stdout
import tables.Tables

main! = || {
	Stdout.line!("b: ${Tables.read_by_app_b.to_str()}")
}
