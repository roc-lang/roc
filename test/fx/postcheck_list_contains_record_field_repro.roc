app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Point : { x : I64 }

State : { values : List(Point) }

initial_state : State
initial_state = { values: [{ x: 1 }] }

main! : () => {}
main! = || {
    Stdout.line!(render(initial_state))
}

render : State -> Str
render = |state| {
    point = { x: 1 }

    if List.contains(state.values, point) {
        "x"
    } else {
        "."
    }
}
