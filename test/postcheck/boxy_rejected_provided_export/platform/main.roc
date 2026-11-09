platform ""
	requires {
		[State : state] for main : {
			init : U64 -> state,
			value : state -> U64,
		}
	}
	exposes []
	packages {}
	provides { "roc_read": read_for_host }
	targets: {
		inputs_dir: "targets/",
		x64mac: { inputs: [app], output: Archive },
		arm64mac: { inputs: [app], output: Archive },
		x64musl: { inputs: [app], output: Archive },
		x64v1musl: { inputs: [app], output: Archive },
		arm64musl: { inputs: [app], output: Archive },
		arm64v1musl: { inputs: [app], output: Archive },
		x64glibc: { inputs: [app], output: Archive },
		arm64glibc: { inputs: [app], output: Archive },
		x64win: { inputs: [app], output: Archive },
		arm64win: { inputs: [app], output: Archive },
	}

# Checking rejects this provided export: its annotation promises a `U64`
# while the body produces a `Str`. The build still succeeds, and the export
# crashes with the checked runtime error if the host calls it.
read_for_host : U64 -> U64
read_for_host = |seed| {
	init_fn = main.init
	value_fn = main.value

	value_fn(init_fn(seed)).to_str()
}
