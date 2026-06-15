platform "json-decoder"
	requires {
		main! : Json => U64
	}
	exposes [Json]
	packages {}
	provides { "roc_main": main_for_host! }
	targets: {
		inputs: "targets/",
		x64mac: { inputs: ["libhost.a", app], output: Exe },
		arm64mac: { inputs: ["libhost.a", app], output: Exe },
		x64musl: { inputs: ["libhost.a", app], output: Exe },
		arm64musl: { inputs: ["libhost.a", app], output: Exe },
		x64win: { inputs: ["host.lib", app], output: Exe },
		arm64win: { inputs: ["host.lib", app], output: Exe },
	}

import Json

main_for_host! : Json => U64
main_for_host! = |json| main!(json)
