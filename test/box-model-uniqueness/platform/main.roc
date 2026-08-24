platform ""
    requires {
        [Model : model] for main : {
            init : {} -> model,
            init_append : {} -> model,
            update : model -> model,
            update_append : model -> model,
            update_pattern : model -> { model : model, effects : List([Observe]) },
            update_erased : model -> { model : model, apply : model -> model },
            cursor : model -> U64,
        }
    }
    exposes [Host]
    packages {}
    provides {
        "roc_init": init_for_host,
        "roc_init_append": init_append_for_host,
        "roc_update_straight": update_straight_for_host,
        "roc_update_adapter": update_adapter_for_host!,
        "roc_update_append": update_append_for_host!,
        "roc_update_pattern": update_pattern_for_host!,
        "roc_update_erased": update_erased_for_host!,
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
        x64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
        arm64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
    }

import Host

init_for_host : {} -> Box(Model)
init_for_host = |{}| Box.box((main.init)({}))

init_append_for_host : {} -> Box(Model)
init_append_for_host = |{}| Box.box((main.init_append)({}))

update_straight_for_host : Box(Model) -> Box(Model)
update_straight_for_host = |boxed| Box.box((main.update)(Box.unbox(boxed)))

update_adapter_for_host! : Box(Model) => Box(Model)
update_adapter_for_host! = |boxed| {
    model = Box.unbox(boxed)
    next = if Host.branch!() (main.update)(model) else (main.update)((main.update)(model))
    Box.box(next)
}

update_append_for_host! : Box(Model) => Box(Model)
update_append_for_host! = |boxed| {
    model = Box.unbox(boxed)
    next = if Host.branch!() (main.update_append)(model) else model
    Box.box(next)
}

update_pattern_for_host! : Box(Model) => Box(Model)
update_pattern_for_host! = |boxed| {
    model = Box.unbox(boxed)
    step = (main.update_pattern)(model)
    next = if Host.branch!() {
        if List.len(step.effects) > 0 step.model else step.model
    } else {
        step.model
    }
    Box.box(next)
}

# LLVM regression: the dead Box is normalized to borrowed-unbox + explicit RC
# before this branch, while the selected arm performs an erased callable call
# whose fixed-size ABI descriptor scratch must live in the procedure entry
# frame rather than being allocated lazily in the arm.
update_erased_for_host! : Box(Model) => Box(Model)
update_erased_for_host! = |boxed| {
    model = Box.unbox(boxed)
    step = (main.update_erased)(model)
    next = if Host.branch!() (step.apply)(step.model) else step.model
    Box.box(next)
}

cursor_for_host : Box(Model) -> U64
cursor_for_host = |boxed| (main.cursor)(Box.unbox(boxed))
