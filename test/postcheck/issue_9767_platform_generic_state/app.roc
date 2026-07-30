app [State, program] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/9767
#
# Checking/building this app should succeed; the compiler must not crash while
# relating the platform's abstract State requirement to the app's concrete
# State(Model).

import pf.App
import pf.Host exposing [Host]

State(model) : [ProgramState(model)]

Model : { count : I32 }

init : {} -> Model
init = |_| { count: 0 }

init! = App.init(App.default, |_host| Ok(ProgramState(init({}))))

render! : State(Model), Host => Try(State(Model), [Exit(I64), ..])
render! = |state, _host| {
    Ok(state)
}

program = { init!, render! }
