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
    }
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

import Msg exposing [Msg]
import View exposing [View]

init_for_host : {} -> Box(Model)
init_for_host = |{}| Box.box(program.init({}))

update_for_host! : Box(Model), Msg => Box(Model)
update_for_host! = |boxed_model, msg| {
    model = Box.unbox(boxed_model)
    Box.box((program.update!)(model, msg))
}

render_for_host : Box(Model) -> View(Model)
render_for_host = |boxed_model| {
    model = Box.unbox(boxed_model)
    program.render(model)
}
