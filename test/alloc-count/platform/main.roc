platform ""
    requires {} { run! : Str => Str }
    exposes [Host]
    packages {}
    provides { "roc_run": run_for_host! }
    hosted {
        "roc_host_alloc_count": Host.alloc_count!,
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

import Host

run_for_host! : Str => Str
run_for_host! = |input| run!(input)
