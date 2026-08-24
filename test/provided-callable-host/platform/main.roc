platform ""
	requires {
		make_boxed_callable : U64 -> Box(U64 -> U64),
		drop_boxed_callable : Box(U64 -> U64) -> {},
		make_aliased_boxed_callables : () -> Box({ first : Box(U64 -> U64), second : Box(U64 -> U64) }),
		make_shared_boxed_callables : () -> Box({ first : Box(U64 -> U64), second : Box(U64 -> U64) }),
		drop_aliased_boxed_callables : Box({ first : Box(U64 -> U64), second : Box(U64 -> U64) }) -> {}
	}
	exposes []
	packages {}
	provides {
		"roc_make_boxed_callable": make_boxed_callable_for_host,
		"roc_drop_boxed_callable": drop_boxed_callable_for_host,
		"roc_make_aliased_boxed_callables": make_aliased_boxed_callables_for_host,
		"roc_make_shared_boxed_callables": make_shared_boxed_callables_for_host,
		"roc_drop_aliased_boxed_callables": drop_aliased_boxed_callables_for_host,
	}
	targets: {
		inputs_dir: "targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
		x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		x64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		arm64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		x64win: { inputs: ["host.lib", app] },
		arm64win: { inputs: ["host.lib", app] },
		wasm32: {
			inputs: ["host.wasm", app],
			exports: ["wasm_main", "wasm_result_len", "wasm_reset_alloc_counts", "wasm_alloc_count", "wasm_dealloc_count"],
		},
	}

make_boxed_callable_for_host : U64 -> Box(U64 -> U64)
make_boxed_callable_for_host = make_boxed_callable

drop_boxed_callable_for_host : Box(U64 -> U64) -> {}
drop_boxed_callable_for_host = drop_boxed_callable

make_aliased_boxed_callables_for_host : () -> Box({ first : Box(U64 -> U64), second : Box(U64 -> U64) })
make_aliased_boxed_callables_for_host = make_aliased_boxed_callables

make_shared_boxed_callables_for_host : () -> Box({ first : Box(U64 -> U64), second : Box(U64 -> U64) })
make_shared_boxed_callables_for_host = make_shared_boxed_callables

drop_aliased_boxed_callables_for_host : Box({ first : Box(U64 -> U64), second : Box(U64 -> U64) }) -> {}
drop_aliased_boxed_callables_for_host = drop_aliased_boxed_callables
