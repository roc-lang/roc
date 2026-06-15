platform "http-headers"
	requires {
		main! : Headers => U64
	}
	exposes [Headers, Decoder]
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

import Decoder
import Headers

main_for_host! : Headers => U64
main_for_host! = |headers| main!(headers)
