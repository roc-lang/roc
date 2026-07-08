platform "http-headers"
	requires {
		main! : Str => U64
	}
	exposes []
	packages {}
	provides { "roc_main": main_for_host! }
	targets: {
		inputs_dir: "targets/",
		x64mac: { inputs: ["libhost.a", app], output: Exe },
		arm64mac: { inputs: ["libhost.a", app], output: Exe },
		x64musl: { inputs: ["libhost.a", app], output: Exe },
		arm64musl: { inputs: ["libhost.a", app], output: Exe },
		x64win: { inputs: ["host.lib", app], output: Exe },
		arm64win: { inputs: ["host.lib", app], output: Exe },
	}

main_for_host! : Str => U64
main_for_host! = |headers| main!(headers)
