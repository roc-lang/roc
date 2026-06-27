app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

increment_i64 : I64 -> I64
increment_i64 = |current| current + 1

initial_components : List(Str)
initial_components = ["Alpha", "Beta"]

reordered_components : List(Str)
reordered_components = ["Beta", "Alpha"]

page_class : Str
page_class = "grid gap-6"

hero_class : Str
hero_class = "grid gap-2 rounded-lg border border-lime-200 bg-lime-50 p-5"

panel_class : Str
panel_class = "grid gap-4"

primary_button_class : Str
primary_button_class = "border-lime-700 bg-lime-600 text-white hover:border-lime-800 hover:bg-lime-700"

quiet_button_class : Str
quiet_button_class = "border-zinc-300 bg-zinc-50 text-zinc-800 hover:border-zinc-400 hover:bg-white"

visible_components : Bool, List(Str) -> List(Str)
visible_components = |show_beta, labels| if show_beta {
	labels
} else {
	["Alpha"]
}

counter_component : Str -> Elem
counter_component = |label| {
	initial_count : I64
	initial_count = 0

	Ui.component(
		|_| {
			Ui.state(
				initial_count,
				|count| {
					count_label =
						Signal.map(
							count.signal(),
							|value| concat3(label, " count: ", value.to_str()),
						)

					Html.section(
						label,
						[],
						[
							Html.button(Str.concat("Increment ", label), count.on_unit(increment_i64)),
							Html.text_s(count_label),
						],
					)
				},
			)
		},
	)
}

render_component : Str, Signal.Signal(Str) -> Elem
render_component = |label, _label_signal| counter_component(label)

main : {} -> Elem
main = |_| {
	Ui.state(
		initial_components,
		|order| {
			Ui.state(
				True,
				|show_beta| {
					visible =
						Signal.map2(
							show_beta.signal(),
							order.signal(),
							visible_components,
						)

					Html.div_c(
						page_class,
						[
							Html.div_c(
								hero_class,
								[
									Html.heading_c("Component composition", "text-3xl font-semibold text-zinc-950"),
									Html.paragraph_c("Reusable component scopes keep local state attached to each keyed component as rows move and disappear.", "max-w-3xl text-sm text-zinc-700"),
								],
							),
							Html.section_c(
								"Component controls",
								panel_class,
								[
									Html.button_c("Reorder components", primary_button_class, order.on_unit(|_| reordered_components)),
									Html.button("Reset order", order.on_unit(|_| initial_components)),
									Html.button_c("Toggle Beta", quiet_button_class, show_beta.on_unit(|value| !value)),
								],
							),
							Ui.each_str(visible, |label| label, render_component),
						],
					)
				},
			)
		},
	)
}
