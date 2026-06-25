app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

label_count : I64 -> Str
label_count = |value| Str.concat("Count: ", value.to_str())

page_class : Str
page_class = "grid gap-6"

hero_class : Str
hero_class = "grid gap-2 rounded-lg border border-emerald-200 bg-emerald-50 p-5"

primary_button_class : Str
primary_button_class = "border-emerald-600 bg-emerald-600 text-white hover:border-emerald-700 hover:bg-emerald-700"

main : {} -> Elem
main = |_| {
	Ui.state(
		0,
		|count| {
			label = Signal.map(count.signal(), label_count)

			Html.div_c(
				page_class,
				[
					Html.div_c(
						hero_class,
						[
							Html.heading_c("Signals counter", "text-3xl font-semibold text-zinc-950"),
							Html.paragraph_c("A tiny retained-state counter for browser runtime checks.", "max-w-3xl text-sm text-zinc-700"),
						],
					),
					Html.button("Decrement", count.on_unit(|value| value - 1)),
					Html.text_s(label),
					Html.button_c("Increment", primary_button_class, count.on_unit(|value| value + 1)),
				],
			)
		},
	)
}
