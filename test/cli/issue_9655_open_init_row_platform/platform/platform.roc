platform ""
    requires {
        [Model : model] for program : {
            init! : {
                config : {},
                run! : {} => Try(model, [Exit(I64), ..]),
            },
        }
    }
    exposes [App]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs: "../platform/targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }

main_for_host! : () => Try(Box(Model), I64)
main_for_host! = || {
    match (program.init!.run!)({}) {
        Ok(model) => Ok(Box.box(model))
        Err(Exit(code)) => Err(code)
        Err(_) => Err(-1)
    }
}

import App
