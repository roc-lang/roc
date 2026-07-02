platform ""
    requires {
        [Model : model] for program : {
            init! : { config : {}, run! : {} => model },
        }
    }
    exposes [App]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "../platform/targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }
main_for_host! : () => Box(Model)
main_for_host! = || {
    Box.box((program.init!.run!)({}))
}
import App
