app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Model : { count: I64 }
Handler : (Model -> Model)

call_boxed_model : Box(Handler), Box(Model) -> Model
call_boxed_model = |handlerBox, modelBox| {
    handler = Box.unbox(handlerBox)
    model = Box.unbox(modelBox)
    handler(model)
}

call_unbox_model_first : Box(Handler), Box(Model) -> Model
call_unbox_model_first = |handlerBox, modelBox| {
    model = Box.unbox(modelBox)
    handler = Box.unbox(handlerBox)
    handler(model)
}

main! = || {

    handler : Handler
    handler = |model| { count: model.count + 1 }

    handlerBox = Box.box(handler)
    modelBox = Box.box({ count: 0 })

    result1 = call_boxed_model(handlerBox, modelBox)
    result2 = call_unbox_model_first(handlerBox, modelBox)

    if result1.count == 1 and result2.count == 1 {
        Stdout.line!("PASS")
    } else {
        Stdout.line!("FAIL: expected 1, got ${Str.inspect(result1.count)} and ${Str.inspect(result2.count)}")
    }
}
