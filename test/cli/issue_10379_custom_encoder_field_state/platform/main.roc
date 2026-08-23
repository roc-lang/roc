platform ""
	requires {
		main! : () => {}
	}
	exposes [FieldFormat, Stdout]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted {
		"roc_stdout_line": Stdout.line!,
	}
	targets: {
		inputs_dir: "../../../fx/platform/targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
		x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		x64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		arm64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		x64win: { inputs: ["host.lib", app] },
		arm64win: { inputs: ["host.lib", app] },
	}

import FieldFormat
import Stdout

main_for_host! : () => {}
main_for_host! = main!
