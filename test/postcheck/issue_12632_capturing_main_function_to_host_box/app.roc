app [State, main] { pf: platform "./platform/main.roc" }

# repro for https://github.com/roc-lang/roc/issues/12632
# `render` captures the top-level constant `label`; the platform's `reload!`
# passes it (read from `main`) into a host-stored boxed closure.
# `roc build --opt=dev` must succeed.

import pf.Prog

State : { count : U64 }

label = "count"

render : State -> Str
render = |state| "${label} ${state.count.to_str()}"

main : Prog.Prog(State)
main = Prog.run({ init: { count: 0 }, render })
