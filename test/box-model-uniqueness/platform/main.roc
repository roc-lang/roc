platform ""
    requires {
        [Model : model] for main : {
            init : {} -> model,
            update : model -> model,
            cursor : model -> U64,
        }
    }
    exposes [Host]
    packages {}
    provides {
        "roc_init": init_for_host,
        "roc_update_straight": update_straight_for_host,
        "roc_update_adapter": update_adapter_for_host!,
        "roc_cursor": cursor_for_host,
    }
    hosted { "roc_host_branch": Host.branch! }
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
    }

import Host

init_for_host : {} -> Box(Model)
init_for_host = |{}| Box.box((main.init)({}))

update_straight_for_host : Box(Model) -> Box(Model)
update_straight_for_host = |boxed| Box.box((main.update)(Box.unbox(boxed)))

update_adapter_for_host! : Box(Model) => Box(Model)
update_adapter_for_host! = |boxed| {
    model = Box.unbox(boxed)
    next = if Host.branch!() (main.update)(model) else (main.update)((main.update)(model))
    Box.box(next)
}

cursor_for_host : Box(Model) -> U64
cursor_for_host = |boxed| (main.cursor)(Box.unbox(boxed))
