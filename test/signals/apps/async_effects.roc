app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

panel = |query, task| {
	ticks = Signal.interval(1000)
	tick_text = Signal.map(ticks, |n| concat3("Ticks: ", n.to_str(), ""))
	status_text =
		Signal.fold_task(
			task,
			"Status: loading",
			|value| concat3("Status: done ", value, ""),
			|err| concat3("Status: failed ", err, ""),
		)

	Html.section(
		"Async panel",
		[],
		[
			Html.text_input("Search", query.signal(), query.on_str(|_, value| value)),
			Html.text_s(status_text),
			Html.text_s(tick_text),
			Ui.on_change(query.signal(), |value| Signal.start_str(task, value)),
			Ui.on_cleanup(Signal.cleanup("async panel cleanup")),
		],
	)
}

main : {} -> Elem
main = |_| {
	Ui.state(
		True,
		|show_panel| {
			Ui.state(
				"",
				|query| {
					task = Signal.fake_task("lookup", |value| value, |err| err)

					Html.div(
						[],
						[
							Html.heading("Async effects"),
							Html.button("Toggle panel", show_panel.on_unit(|value| !value)),
							Ui.when(
								show_panel.signal(),
								|_| panel(query, task),
								|_| Html.section("Panel closed", [], [Html.text("Closed")]),
							),
						],
					)
				},
			)
		},
	)
}
