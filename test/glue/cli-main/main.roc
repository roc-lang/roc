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
        "roc_cli_wide": CliHost.wide!,
    }
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

import CliHost

main_for_host! : List(Str) => Try({}, [Exit(I32)])
main_for_host! = main!
