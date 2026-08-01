platform ""
	requires {
		make_boxed_callable : U64 -> Box(U64 -> U64),
		drop_boxed_callable : Box(U64 -> U64) -> {}
	}
	exposes []
	packages {}
	provides {
		"roc_make_boxed_callable": make_boxed_callable_for_host,
		"roc_drop_boxed_callable": drop_boxed_callable_for_host,
	}
	targets: {
		inputs_dir: "targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
		x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		x64win: { inputs: ["host.lib", app] },
		arm64win: { inputs: ["host.lib", app] },
	}

make_boxed_callable_for_host : U64 -> Box(U64 -> U64)
make_boxed_callable_for_host = make_boxed_callable

drop_boxed_callable_for_host : Box(U64 -> U64) -> {}
drop_boxed_callable_for_host = drop_boxed_callable
