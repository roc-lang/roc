app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

main : {} -> Elem
main = |_| {
	Ui.state(
		0,
		|count| {
			stable = Signal.map(count.signal(), |_| "Stable")

			Html.div(
				[],
				[
					Html.button("Bump", count.on_unit(|value| value + 1)),
					Html.text_s(stable),
				],
			)
		},
	)
}
