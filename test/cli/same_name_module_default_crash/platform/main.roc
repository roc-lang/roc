platform ""
	requires {
		main! : () => Try(_, [Exit(I8), ..])
	}
	exposes [Cfg]
	packages {}
	provides { "roc_main": main_for_host! }

import Cfg

main_for_host! : {} => I8
main_for_host! = |_|
	match main!() {
		Ok(_) => 0
		Err(Exit(code)) => code
		Err(_) => 1
	}
