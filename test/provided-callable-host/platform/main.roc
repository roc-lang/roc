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
		x64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
		arm64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
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
