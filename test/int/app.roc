app [init, update, render] { pf: platform "./platform/main.roc" }

Model : { value: I64 }

init : {} -> Model
init = |{}| { value: 0 }

update : Model, I64 -> Model
update = |m, delta| { value: m.value + delta }

render : Model -> I64
render = |m| m.value
