platform ""
    requires {
        [Model : model] for main : {
            init : {} -> model,
            update : model, I64 -> model,
            render : model -> I64
        }
    }
    exposes []
    packages {}
    provides { init_for_host: "init", update_for_host: "update", render_for_host: "render" }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64glibc: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"],
            arm64glibc: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"],
        }
    }

# Explicit type annotations for host-facing functions
# Note: Use uppercase Model here - it's a type alias introduced by the for-clause [Model : model]
# that gets unified with the app's concrete type during type checking.
init_for_host : {} -> Box(Model)
init_for_host = |{}| {
    init_fn = main.init
    record = init_fn({})
    Box.box(record)
}

update_for_host : Box(Model), I64 -> Box(Model)
update_for_host = |boxed_model, value| {
    m = Box.unbox(boxed_model)
    update_fn = main.update
    Box.box(update_fn(m, value))
}

render_for_host : Box(Model) -> I64
render_for_host = |boxed_model| {
    m = Box.unbox(boxed_model)
    render_fn = main.render
    render_fn(m)
}
