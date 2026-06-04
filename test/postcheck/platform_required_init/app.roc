app [Model, program] { pf: platform "./platform/main.roc" }

import pf.Host

Model : {
    mouse_x : F32,
    value : Str,
}

program = { init!, render! }

init! : Host => Try(Model, [Exit(I64), NotFound, ..])
init! = |host| {
    value = host.read_env!("ROC_POSTCHECK_REGRESSION")?

    Ok({
        mouse_x: host.mouse.x,
        value,
    })
}

render! : Model, Host => Try(Model, [Exit(I64), ..])
render! = |model, _host| Ok(model)
