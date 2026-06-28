app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

BoundaryState : {
	query : Str,
	last_key : Str,
	shift_key : Bool,
	submits : I64,
}

initial_state : BoundaryState
initial_state = {
	query: "",
	last_key: "none",
	shift_key: False,
	submits: 0,
}

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

concat4 : Str, Str, Str, Str -> Str
concat4 = |a, b, c, d| Str.concat(concat3(a, b, c), d)

set_query : BoundaryState, Str -> BoundaryState
set_query = |state, value| { ..state, query: value }

set_key : BoundaryState, Ui.KeyPayload -> BoundaryState
set_key = |state, payload| {
	{ ..state, last_key: payload.key, shift_key: payload.shift_key }
}

record_submit : BoundaryState -> BoundaryState
record_submit = |state| { ..state, submits: state.submits + 1 }

key_label : BoundaryState -> Str
key_label = |state| {
	shift_label =
		if state.shift_key {
			"(shift)"
		} else {
			"(plain)"
		}

	concat4("Last key: ", state.last_key, " ", shift_label)
}

submit_label : BoundaryState -> Str
submit_label = |state| Str.concat("Submits: ", state.submits.to_str())

input_class : Str
input_class = "w-full max-w-md"

main : {} -> Elem
main = |_| {
	Ui.state(
		initial_state,
		|model| {
			state_signal = model.signal()
			query_signal = Signal.map(state_signal, |state| state.query)
			key_signal = Signal.map(state_signal, key_label)
			submit_signal = Signal.map(state_signal, submit_label)

			Html.section(
				"Event payload boundary",
				[Html.attr("data-boundary", "events")],
				[
					Html.heading_c("Signals event boundary", "text-2xl font-semibold text-zinc-950"),
					Html.link(
						"Boundary guide",
						[
							Html.attr("href", "/signals/boundary"),
							Html.attr("aria-label", "Boundary guide"),
							Html.attr("data-link", "guide"),
						],
					),
					Html.text_input_attrs(
						"Boundary input",
						query_signal,
						[
							Html.class_attr(input_class),
							Html.attr("id", "boundary-input"),
							Html.attr("placeholder", "Type a key"),
							Html.attr("data-static", "ready"),
							Html.attr_s("data-query", query_signal),
							Html.on_key_down(model.on_key(set_key)),
						],
						model.on_str(set_query),
					),
					Html.paragraph_s(key_signal),
					Html.form_label(
						"Boundary form",
						[
							Html.attr("id", "boundary-form"),
							Html.attr("data-form", "event-payload"),
							Html.on_submit_prevent_default(model.on_unit(record_submit)),
						],
						[
							Html.paragraph_s(submit_signal),
							Html.button("Submit boundary", model.on_unit(record_submit)),
						],
					),
				],
			)
		},
	)
}
