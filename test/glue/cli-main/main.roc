platform ""
    requires {
        main! : List(Str) => Try({}, [Exit(I32)])
    }
    exposes [CliHost]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_cli_log": CliHost.log!,
        "roc_cli_read": CliHost.read!,
        "roc_cli_many": CliHost.many!,
        "roc_cli_shape": CliHost.shape!,
        "roc_cli_checksum": CliHost.checksum!,
        "roc_cli_wide": CliHost.wide!,
    }
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"], output: Exe },
        x64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"], output: Exe },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"], output: Exe },
        arm64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"], output: Exe },
        wasm32: {
            inputs: ["host.wasm", app],
            output: Shared,
            exports: ["wasm_main", "wasm_result_len", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

import CliHost

main_for_host! : List(Str) => Try({}, [Exit(I32)])
main_for_host! = main!
