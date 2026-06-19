import Elem exposing [Elem]
import Node
import Signal exposing [Signal]

## Static UI structure and attributes. Markup carries no identity; dynamic text
## and attributes reference signals, and event handlers carry reducer messages.
Html := [].{
	Attr : Node.Attr

	div : List(Node.Attr), List(Elem) -> Elem
	div = |attrs, children| Elem.Element({ tag: "div", attrs, children })

	section : Str, List(Node.Attr), List(Elem) -> Elem
	section = |label, attrs, children| {
		base = [
			Node.Attr.StaticText({ field: Node.field_role, value: "region" }),
			Node.Attr.StaticText({ field: Node.field_label, value: label }),
		]
		Elem.Element({ tag: "section", attrs: List.concat(base, attrs), children })
	}

	heading : Str -> Elem
	heading = |text_value| {
		Elem.Element(
			{
				tag: "h2",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_role, value: "heading" }),
					Node.Attr.StaticText({ field: Node.field_text, value: text_value }),
				],
				children: [],
			},
		)
	}

	paragraph : Str -> Elem
	paragraph = |text_value| {
		Elem.Element(
			{
				tag: "p",
				attrs: [Node.Attr.StaticText({ field: Node.field_text, value: text_value })],
				children: [],
			},
		)
	}

	text : Str -> Elem
	text = |value| Elem.Text(value)

	## Signal-backed text content.
	text_s : Signal(Str) -> Elem
	text_s = |signal| Elem.TextSignal(Signal.to_expr(signal))

	## A button whose label is static text and whose click fires `msg`.
	button : Str, Node.Msg -> Elem
	button = |label, msg| {
		Elem.Element(
			{
				tag: "button",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_text, value: label }),
					Node.Attr.OnEvent({ kind: Node.event_kind_click, msg }),
				],
				children: [],
			},
		)
	}

	## A button whose label is signal-backed.
	button_s : Signal(Str), Node.Msg -> Elem
	button_s = |label, msg| {
		Elem.Element(
			{
				tag: "button",
				attrs: [
					Node.Attr.SignalText({ field: Node.field_text, signal: Signal.to_expr(label) }),
					Node.Attr.OnEvent({ kind: Node.event_kind_click, msg }),
				],
				children: [],
			},
		)
	}

	## A button whose label and disabled state are signal-backed.
	action_button : Signal(Str), Signal(Bool), Node.Msg -> Elem
	action_button = |label, disabled, msg| {
		Elem.Element(
			{
				tag: "button",
				attrs: [
					Node.Attr.SignalText({ field: Node.field_text, signal: Signal.to_expr(label) }),
					Node.Attr.SignalBool({ field: Node.bool_field_disabled, signal: Signal.to_expr(disabled) }),
					Node.Attr.OnEvent({ kind: Node.event_kind_click, msg }),
				],
				children: [],
			},
		)
	}

	## A text input bound to a signal value, firing `msg` (a str-payload reducer)
	## on input.
	text_input : Str, Signal(Str), Node.Msg -> Elem
	text_input = |label, value, msg| {
		Elem.Element(
			{
				tag: "input",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_role, value: "textbox" }),
					Node.Attr.StaticText({ field: Node.field_label, value: label }),
					Node.Attr.SignalText({ field: Node.field_value, signal: Signal.to_expr(value) }),
					Node.Attr.OnEvent({ kind: Node.event_kind_input, msg }),
				],
				children: [],
			},
		)
	}

	## A checkbox bound to a signal value, firing `msg` (a bool-payload reducer) on
	## change.
	checkbox : Str, Signal(Bool), Node.Msg -> Elem
	checkbox = |label, checked, msg| {
		Elem.Element(
			{
				tag: "input",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_role, value: "checkbox" }),
					Node.Attr.StaticText({ field: Node.field_label, value: label }),
					Node.Attr.SignalBool({ field: Node.bool_field_checked, signal: Signal.to_expr(checked) }),
					Node.Attr.OnEvent({ kind: Node.event_kind_check, msg }),
				],
				children: [],
			},
		)
	}
}
