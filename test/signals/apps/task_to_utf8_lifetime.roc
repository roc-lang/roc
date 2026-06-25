app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Http
import pf.Signal
import pf.Ui

TaskState := [Loading, Ready(U64), Failed(U64)]

decode : Str -> TaskState
decode = |body| Ready(List.len(Str.to_utf8(body)))

failed : Str -> TaskState
failed = |err| Failed(List.len(Str.to_utf8(err)))

state_text : TaskState -> Str
state_text = |state|
	match state {
		Loading => "loading"
		Ready(n) => "ready bytes ${n.to_str()}"
		Failed(n) => "failed bytes ${n.to_str()}"
	}

main : {} -> Elem
main = |_| {
	task = Http.get_text_task("dashboard")
	state = Signal.fold_task(task, Loading, decode, failed)
	text = Signal.map(state, state_text)

	Html.div_c(
		"",
		[
			Html.heading("Task UTF-8 lifetime"),
			Html.text_s(text),
			Ui.on_mount(|_| Http.get_text(task, "/api/ops/dashboard")),
		],
	)
}
