app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

page_class : Str
page_class = "grid gap-6"

hero_class : Str
hero_class = "grid gap-2 rounded-lg border border-zinc-200 bg-white p-5"

main : {} -> Elem
main = |_| {
	Ui.state(
		0,
		|count| {
			stable = Signal.map(count.signal(), |_| "Stable")

			Html.div_c(
				page_class,
				[
					Html.div_c(
						hero_class,
						[
							Html.heading_c("Stable text", "text-3xl font-semibold text-zinc-950"),
							Html.paragraph_c("The signal output remains text-identical across updates.", "max-w-3xl text-sm text-zinc-700"),
						],
					),
					Html.button("Bump", count.on_unit(|value| value + 1)),
					Html.text_s(stable),
				],
			)
		},
	)
}
