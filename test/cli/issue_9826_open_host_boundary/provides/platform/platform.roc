platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }

main_for_host! = main!
