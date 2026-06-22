app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

label_count : I64 -> Str
label_count = |value| Str.concat("Count: ", value.to_str())

main : {} -> Elem
main = |_| {
	Ui.state(
		0,
		|count| {
			label = Signal.map(count.signal(), label_count)

			Html.div(
				[],
				[
					Html.heading("Signals counter"),
					Html.button("Decrement", count.on_unit(|value| value - 1)),
					Html.text_s(label),
					Html.button("Increment", count.on_unit(|value| value + 1)),
				],
			)
		},
	)
}
