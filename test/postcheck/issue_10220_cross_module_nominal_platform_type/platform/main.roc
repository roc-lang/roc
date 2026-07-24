platform ""
	requires {} {
		main : {
			things : List(Box({ ctx : U64 } => Lib.Thing)),
		},
	}
	exposes [Lib]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted {}
	targets: {
		inputs_dir: "targets/",
		arm64mac: { inputs: [app], output: Archive },
		x64mac: { inputs: [app], output: Archive },
	}

import Lib

main_for_host! : {} => {}
main_for_host! = |_| {}
