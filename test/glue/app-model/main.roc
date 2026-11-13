platform ""
    requires {
        [Model : model] for program : {
            init : {} -> model,
            update! : model, Msg => model,
            render : model -> View(model),
        }
    }
    exposes [Msg, View]
    packages {}
    provides {
        "roc_init": init_for_host,
        "roc_update": update_for_host!,
        "roc_render": render_for_host,
        "roc_release_model": release_model_for_host,
    }
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: ["rcrt1.o", "libhost.a", app, "libc.a"], output: Exe },
        x64v1musl: { inputs: ["rcrt1.o", "libhost.a", app, "libc.a"], output: Exe },
        arm64musl: { inputs: ["rcrt1.o", "libhost.a", app, "libc.a"], output: Exe },
        arm64v1musl: { inputs: ["rcrt1.o", "libhost.a", app, "libc.a"], output: Exe },
        wasm32: {
            inputs: ["host.wasm", app],
            output: Shared,
            exports: ["wasm_main", "wasm_result_len", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

import Msg
import View

init_for_host : {} -> Box(Model)
init_for_host = |{}| Box.box((program.init)({}))

update_for_host! : Box(Model), Msg => Box(Model)
update_for_host! = |boxed_model, msg| {
    model = Box.unbox(boxed_model)
    Box.box((program.update!)(model, msg))
}

render_for_host : Box(Model) -> View(Model)
render_for_host = |boxed_model| {
    model = Box.unbox(boxed_model)
    (program.render)(model)
}

## The host cannot release a `Box(Model)`: only Roc knows the model's layout.
## It hands the box to this function instead, which takes ownership and drops
## it.
release_model_for_host : Box(Model) -> {}
release_model_for_host = |_| {}
