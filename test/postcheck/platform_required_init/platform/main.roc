platform ""
    requires {
        [Model : model] for program : {
            init! : Host => Try(model, [Exit(I64), ..]),
            render! : model, Host => Try(model, [Exit(I64), ..]),
        }
    }
    exposes [Host]
    packages {}
    provides {
        "roc_init_for_host": init_for_host!,
        "roc_render_for_host": render_for_host!,
    }
    hosted {
        "roc_host_read_env": Host.read_env!,
        "roc_host_set_mouse": Host.set_mouse!,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: [app], output: Archive },
        arm64mac: { inputs: [app], output: Archive },
        x64musl: { inputs: [app], output: Archive },
        arm64musl: { inputs: [app], output: Archive },
        x64glibc: { inputs: [app], output: Archive },
        arm64glibc: { inputs: [app], output: Archive },
        x64win: { inputs: [app], output: Archive },
        arm64win: { inputs: [app], output: Archive },
    }

import Host

HostStateFromHost : {
    env_prefix : Str,
    mouse_x : F32,
    mouse_y : F32,
}

init_for_host! : HostStateFromHost => Try(Box(Model), I64)
init_for_host! = |host_state| {
    host = {
        env_prefix: host_state.env_prefix,
        mouse: {
            x: host_state.mouse_x,
            y: host_state.mouse_y,
        },
    }

    match (program.init!)(host) {
        Ok(model) => Ok(Box.box(model))
        Err(Exit(code)) => Err(code)
        Err(_) => Err(42)
    }
}

render_for_host! : Box(Model), HostStateFromHost => Try(Box(Model), I64)
render_for_host! = |boxed_model, host_state| {
    host = {
        env_prefix: host_state.env_prefix,
        mouse: {
            x: host_state.mouse_x,
            y: host_state.mouse_y,
        },
    }

    match (program.render!)(Box.unbox(boxed_model), host) {
        Ok(model) => Ok(Box.box(model))
        Err(Exit(code)) => Err(code)
        Err(_) => Err(-1)
    }
}
