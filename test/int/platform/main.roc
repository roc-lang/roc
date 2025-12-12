platform ""
    requires { model } { init : {} -> model, update : model, I64 -> model, render : model -> I64 }
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

# Returns Box(model) - this works (return value)
init_for_host : {} -> Box(model)
init_for_host = |{}| Box.box(init({}))

# Takes Box(model) as parameter - this should trigger the bug
# Also takes I64 which host can provide
update_for_host : Box(model), I64 -> Box(model)
update_for_host = |boxed_model, value| {
    m = Box.unbox(boxed_model)
    Box.box(update(m, value))
}

# Takes Box(model) as parameter, returns I64 for host verification
render_for_host : Box(model) -> I64
render_for_host = |boxed_model| {
    m = Box.unbox(boxed_model)
    render(m)
}
