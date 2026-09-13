platform "reassign-type-error"
	requires {
		target : { run!: List(U8) => U8 }
	}
	exposes []
	packages {}
	provides {
		"run": run_for_host!,
	}
	targets: {
		x64musl: { inputs: [app], output: Archive },
	}

run_for_host! : List(U8) => U8
run_for_host! = |input| target.run!(input)
