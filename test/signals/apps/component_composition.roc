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

					Html.div(
						[],
						[
							Html.heading("Component composition"),
							Html.section(
								"Component controls",
								[],
								[
									Html.button("Reorder components", order.on_unit(|_| reordered_components)),
									Html.button("Reset order", order.on_unit(|_| initial_components)),
									Html.button("Toggle Beta", show_beta.on_unit(|value| !value)),
								],
							),
							Ui.each(visible, |label| label, Ui.str_key_hash, render_component),
						],
					)
				},
			)
		},
	)
}
