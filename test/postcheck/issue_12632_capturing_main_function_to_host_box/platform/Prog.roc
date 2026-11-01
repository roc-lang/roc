import Host
import Session

Prog(state) := { init : state, render : state -> Str }.{
	run : { init : state, render : state -> Str } -> Prog(state)
	run = |config| Prog.(config)

	renderer : Prog(state) -> (state -> Str)
	renderer = |Prog.(config)| config.render

	start! : Prog(state) => {}
	start! = |Prog.(config)| install!(config.init)

	install! : state => {}
	install! = |state| Host.set_dispatch!(Box.box(|_session| install!(state)))
}
