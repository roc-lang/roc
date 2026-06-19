import Node
import Signal exposing [Signal]

## Static UI structure and attributes. Markup carries no identity; dynamic text
## and attributes reference signals, and event handlers carry reducer messages.
Html := [].{
	Elem : Node.Elem
	Attr : Node.Attr

	div : List(Node.Attr), List(Node.Elem) -> Node.Elem
	div = |attrs, children| Node.Elem.Element({ tag: "div", attrs, children })

	section : Str, List(Node.Attr), List(Node.Elem) -> Node.Elem
	section = |label, attrs, children| {
		base = [
			Node.Attr.StaticText({ field: Node.field_role, value: "region" }),
			Node.Attr.StaticText({ field: Node.field_label, value: label }),
		]
		Node.Elem.Element({ tag: "section", attrs: List.concat(base, attrs), children })
	}

	heading : Str -> Node.Elem
	heading = |text_value| {
		Node.Elem.Element(
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

	paragraph : Str -> Node.Elem
	paragraph = |text_value| {
		Node.Elem.Element(
			{
				tag: "p",
				attrs: [Node.Attr.StaticText({ field: Node.field_text, value: text_value })],
				children: [],
			},
		)
	}

	text : Str -> Node.Elem
	text = |value| Node.Elem.Text(value)

	## Signal-backed text content.
	text_s : Signal(Str) -> Node.Elem
	text_s = |signal| Node.Elem.TextSignal(Box.box(Signal.to_expr(signal)))

	## A button whose label is static text and whose click fires `msg`.
	button : Str, Node.Msg -> Node.Elem
	button = |label, msg| {
		Node.Elem.Element(
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
	button_s : Signal(Str), Node.Msg -> Node.Elem
	button_s = |label, msg| {
		Node.Elem.Element(
			{
				tag: "button",
				attrs: [
					Node.Attr.SignalText({ field: Node.field_text, signal: Box.box(Signal.to_expr(label)) }),
					Node.Attr.OnEvent({ kind: Node.event_kind_click, msg }),
				],
				children: [],
			},
		)
	}

	## A text input bound to a signal value, firing `msg` (a str-payload reducer)
	## on input.
	text_input : Str, Signal(Str), Node.Msg -> Node.Elem
	text_input = |label, value, msg| {
		Node.Elem.Element(
			{
				tag: "input",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_role, value: "textbox" }),
					Node.Attr.StaticText({ field: Node.field_label, value: label }),
					Node.Attr.SignalText({ field: Node.field_value, signal: Box.box(Signal.to_expr(value)) }),
					Node.Attr.OnEvent({ kind: Node.event_kind_input, msg }),
				],
				children: [],
			},
		)
	}

	## A checkbox bound to a signal value, firing `msg` (a bool-payload reducer) on
	## change.
	checkbox : Str, Signal(Bool), Node.Msg -> Node.Elem
	checkbox = |label, checked, msg| {
		Node.Elem.Element(
			{
				tag: "input",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_role, value: "checkbox" }),
					Node.Attr.StaticText({ field: Node.field_label, value: label }),
					Node.Attr.SignalBool({ field: Node.bool_field_checked, signal: Box.box(Signal.to_expr(checked)) }),
					Node.Attr.OnEvent({ kind: Node.event_kind_check, msg }),
				],
				children: [],
			},
		)
	}
}
