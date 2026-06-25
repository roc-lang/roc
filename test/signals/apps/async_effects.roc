app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

page_class : Str
page_class = "grid gap-6"

hero_class : Str
hero_class = "grid gap-2 rounded-lg border border-teal-200 bg-teal-50 p-5"

panel_class : Str
panel_class = "grid gap-4"

primary_button_class : Str
primary_button_class = "border-teal-600 bg-teal-600 text-white hover:border-teal-700 hover:bg-teal-700"

input_class : Str
input_class = "w-full max-w-md"

TaskView := [Loading, Done(Str), Failed(Str)]

status_text = |status|
	match status {
		Loading => "Status: loading"
		Done(value) => concat3("Status: done ", value, "")
		Failed(err) => concat3("Status: failed ", err, "")
	}

is_done = |status|
	match status {
		Done(_) => True
		_ => False
	}

is_failed = |status|
	match status {
		Failed(_) => True
		_ => False
	}

done_text = |status|
	match status {
		Done(value) => concat3("Ready payload: ", value, "")
		_ => "Waiting for task result"
	}

failed_text = |status|
	match status {
		Failed(err) => concat3("Failed payload: ", err, "")
		_ => "No task failure"
	}

panel = |query, task| {
	ticks = Signal.interval(1000)
	tick_text = Signal.map(ticks, |n| concat3("Ticks: ", n.to_str(), ""))
	status =
		Signal.fold_task(
			task,
			Loading,
			|value| Done(value),
			|err| Failed(err),
		)
	ready = Signal.map(status, is_done)
	failed = Signal.map(status, is_failed)

	Html.section_c(
		"Async panel",
		panel_class,
		[
			Html.text_input_c("Search", query.signal(), input_class, query.on_str(|_, value| value)),
			Html.text_s(Signal.map(status, status_text)),
			Ui.when(
				ready,
				|_| Html.text_s(Signal.map(status, done_text)),
				|_| Html.text("Waiting for task result"),
			),
			Ui.when(
				failed,
				|_| Html.text_s(Signal.map(status, failed_text)),
				|_| Html.text("No task failure"),
			),
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

					Html.div_c(
						page_class,
						[
							Html.div_c(
								hero_class,
								[
									Html.heading_c("Async effects", "text-3xl font-semibold text-zinc-950"),
									Html.paragraph_c("A deterministic async surface for task lifecycle, interval ticks, cancellation, and cleanup descriptors.", "max-w-3xl text-sm text-zinc-700"),
								],
							),
							Html.button_c("Toggle panel", primary_button_class, show_panel.on_unit(|value| !value)),
							Ui.when(
								show_panel.signal(),
								|_| panel(query, task),
								|_| Html.section_c("Panel closed", panel_class, [Html.text("Closed")]),
							),
						],
					)
				},
			)
		},
	)
}
