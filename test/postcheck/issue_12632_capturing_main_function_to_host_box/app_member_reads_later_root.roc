app [State, main] { pf: platform "./platform/main.roc" }

# A boxed closure in `handlers` reads `handler_count`, which reads `handlers`:
# the stage evaluating `handlers` compiles the closure without running it.

import pf.Prog

State : { count : U64 }

handlers : List(Box((U64 -> U64)))
handlers = [Box.box(|k| k + handler_count)]

handler_count : U64
handler_count = List.len(handlers)

render : State -> Str
render = |state| {
	total = List.fold(handlers, 0.U64, |sum, handler| sum + Box.unbox(handler)(state.count))
	total.to_str()
}

main : Prog.Prog(State)
main = Prog.run({ init: { count: 0 }, render })
