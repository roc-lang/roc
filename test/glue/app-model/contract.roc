app [program] { pf: platform "./main.roc" }

import pf.Msg
import pf.View

## The label is a heap string, so the model's box has refcounted contents:
## releasing it frees two allocations, which only code that knows this layout
## can do.
Model := { count : I32, label : Str }

program = {
    init,
    update!,
    render,
}

init : {} -> Model
init = |_| { count: 1, label: "a label long enough to be stored on the heap" }

update! : Model, Msg => Model
update! = |model, msg| {
    match msg {
        Reset => { count: model.count + 1, label: model.label },
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
