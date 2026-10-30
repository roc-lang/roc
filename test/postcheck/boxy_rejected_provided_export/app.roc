app [State, main] { pf: platform "./platform/main.roc" }

State : { value : U64 }

main = { init, value }

init : U64 -> State
init = |seed| { value: seed }

value : State -> U64
value = |state| state.value
