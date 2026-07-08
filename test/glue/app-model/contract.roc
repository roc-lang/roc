app [program] { pf: platform "./main.roc" }

import pf.Msg exposing [Msg]
import pf.View exposing [View]

Model := { count : I32 }

program = {
    init,
    update!,
    render,
}

init : {} -> Model
init = |_| { count: 1 }

update! : Model, Msg => Model
update! = |model, msg| {
    match msg {
        Reset => { count: model.count + 1 },
        _ => model,
    }
}

render : Model -> View(Model)
render = |model| {
    {
        title: "ready",
        model: Box.box(model),
        messages: [],
        lifecycle: Ready,
    }
}
