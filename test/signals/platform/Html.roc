import Elem exposing [Elem]
import HostValue exposing [HostValue]
import Node
import Signal exposing [Signal]

## Static UI structure and attributes. Markup carries no identity; dynamic text
## and attributes reference signals, and event handlers carry reducer messages.
Html := [].{
	Attr : Node.Attr

	class_attr : Str -> Node.Attr
	class_attr = |value| Node.Attr.StaticText({ field: Node.field_class, value })

	div : List(Node.Attr), List(Elem) -> Elem
	div = |attrs, children| Elem.Element({ tag: "div", attrs, children })

	div_c : Str, List(Elem) -> Elem
	div_c = |classes, children| div([class_attr(classes)], children)

	section : Str, List(Node.Attr), List(Elem) -> Elem
	section = |label, attrs, children| {
		base = [
			Node.Attr.StaticText({ field: Node.field_role, value: "region" }),
			Node.Attr.StaticText({ field: Node.field_label, value: label }),
		]
		Elem.Element({ tag: "section", attrs: List.concat(base, attrs), children })
	}

	section_c : Str, Str, List(Elem) -> Elem
	section_c = |label, classes, children| section(label, [class_attr(classes)], children)

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

	heading_c : Str, Str -> Elem
	heading_c = |text_value, classes| {
		Elem.Element(
			{
				tag: "h2",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_role, value: "heading" }),
					Node.Attr.StaticText({ field: Node.field_text, value: text_value }),
					class_attr(classes),
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

	paragraph_c : Str, Str -> Elem
	paragraph_c = |text_value, classes| {
		Elem.Element(
			{
				tag: "p",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_text, value: text_value }),
					class_attr(classes),
				],
				children: [],
			},
		)
	}

	text : Str -> Elem
	text = |value| Elem.Text(value)

	## Signal-backed text content.
	text_s : Signal(Str) -> Elem
	text_s = |signal| {
		tag = signal.tag
		read : HostValue -> Str
		read = |value| Box.unbox(HostValue.get_tagged(value, tag))
		Elem.TextSignal({ signal: Signal.to_expr(signal), read: Box.box(read) })
	}

	## Signal-backed preformatted text block.
	pre_s_c : Signal(Str), Str -> Elem
	pre_s_c = |signal, classes| {
		tag = signal.tag
		read : HostValue -> Str
		read = |value| Box.unbox(HostValue.get_tagged(value, tag))
		Elem.Element(
			{
				tag: "pre",
				attrs: [
					Node.Attr.SignalText({ field: Node.field_text, signal: Signal.to_expr(signal), read: Box.box(read) }),
					class_attr(classes),
				],
				children: [],
			},
		)
	}

	## A button whose label is static text and whose click fires `msg`.
	button : Str, Node.Msg -> Elem
	button = |label, msg| button_attrs(label, [], msg)

	button_c : Str, Str, Node.Msg -> Elem
	button_c = |label, classes, msg| button_attrs(label, [class_attr(classes)], msg)

	button_attrs : Str, List(Node.Attr), Node.Msg -> Elem
	button_attrs = |label, attrs, msg| {
		Elem.Element(
			{
				tag: "button",
				attrs: List.concat(
					[
						Node.Attr.StaticText({ field: Node.field_text, value: label }),
						Node.Attr.OnEvent({ kind: Node.event_kind_click, msg }),
					],
					attrs,
				),
				children: [],
			},
		)
	}

	## A button whose label is signal-backed.
	button_s : Signal(Str), Node.Msg -> Elem
	button_s = |label, msg| button_s_attrs(label, [], msg)

	button_s_c : Signal(Str), Str, Node.Msg -> Elem
	button_s_c = |label, classes, msg| button_s_attrs(label, [class_attr(classes)], msg)

	button_s_attrs : Signal(Str), List(Node.Attr), Node.Msg -> Elem
	button_s_attrs = |label, attrs, msg| {
		label_tag = label.tag
		read_label : HostValue -> Str
		read_label = |value| Box.unbox(HostValue.get_tagged(value, label_tag))
		Elem.Element(
			{
				tag: "button",
				attrs: List.concat(
					[
						Node.Attr.SignalText({ field: Node.field_text, signal: Signal.to_expr(label), read: Box.box(read_label) }),
						Node.Attr.OnEvent({ kind: Node.event_kind_click, msg }),
					],
					attrs,
				),
				children: [],
			},
		)
	}

	## A button whose label and disabled state are signal-backed.
	action_button : Signal(Str), Signal(Bool), Node.Msg -> Elem
	action_button = |label, disabled, msg| action_button_attrs(label, disabled, [], msg)

	action_button_c : Signal(Str), Signal(Bool), Str, Node.Msg -> Elem
	action_button_c = |label, disabled, classes, msg| action_button_attrs(label, disabled, [class_attr(classes)], msg)

	action_button_attrs : Signal(Str), Signal(Bool), List(Node.Attr), Node.Msg -> Elem
	action_button_attrs = |label, disabled, attrs, msg| {
		label_tag = label.tag
		disabled_tag = disabled.tag
		read_label : HostValue -> Str
		read_label = |value| Box.unbox(HostValue.get_tagged(value, label_tag))
		read_disabled : HostValue -> Bool
		read_disabled = |value| Box.unbox(HostValue.get_tagged(value, disabled_tag))
		Elem.Element(
			{
				tag: "button",
				attrs: List.concat(
					[
						Node.Attr.SignalText({ field: Node.field_text, signal: Signal.to_expr(label), read: Box.box(read_label) }),
						Node.Attr.SignalBool({ field: Node.bool_field_disabled, signal: Signal.to_expr(disabled), read: Box.box(read_disabled) }),
						Node.Attr.OnEvent({ kind: Node.event_kind_click, msg }),
					],
					attrs,
				),
				children: [],
			},
		)
	}

	## A text input bound to a signal value, firing `msg` (a str-payload reducer)
	## on input.
	text_input : Str, Signal(Str), Node.Msg -> Elem
	text_input = |label, value, msg| text_input_attrs(label, value, [], msg)

	text_input_c : Str, Signal(Str), Str, Node.Msg -> Elem
	text_input_c = |label, value, classes, msg| text_input_attrs(label, value, [class_attr(classes)], msg)

	text_input_attrs : Str, Signal(Str), List(Node.Attr), Node.Msg -> Elem
	text_input_attrs = |label, value, attrs, msg| {
		value_tag = value.tag
		read_value : HostValue -> Str
		read_value = |host_value| Box.unbox(HostValue.get_tagged(host_value, value_tag))
		Elem.Element(
			{
				tag: "input",
				attrs: List.concat(
					[
						Node.Attr.StaticText({ field: Node.field_role, value: "textbox" }),
						Node.Attr.StaticText({ field: Node.field_label, value: label }),
						Node.Attr.SignalText({ field: Node.field_value, signal: Signal.to_expr(value), read: Box.box(read_value) }),
						Node.Attr.OnEvent({ kind: Node.event_kind_input, msg }),
					],
					attrs,
				),
				children: [],
			},
		)
	}

	## A checkbox bound to a signal value, firing `msg` (a bool-payload reducer) on
	## change.
	checkbox : Str, Signal(Bool), Node.Msg -> Elem
	checkbox = |label, checked, msg| checkbox_attrs(label, checked, [], msg)

	checkbox_c : Str, Signal(Bool), Str, Node.Msg -> Elem
	checkbox_c = |label, checked, classes, msg| checkbox_attrs(label, checked, [class_attr(classes)], msg)

	checkbox_attrs : Str, Signal(Bool), List(Node.Attr), Node.Msg -> Elem
	checkbox_attrs = |label, checked, attrs, msg| {
		checked_tag = checked.tag
		read_checked : HostValue -> Bool
		read_checked = |value| Box.unbox(HostValue.get_tagged(value, checked_tag))
		Elem.Element(
			{
				tag: "input",
				attrs: List.concat(
					[
						Node.Attr.StaticText({ field: Node.field_role, value: "checkbox" }),
						Node.Attr.StaticText({ field: Node.field_label, value: label }),
						Node.Attr.SignalBool({ field: Node.bool_field_checked, signal: Signal.to_expr(checked), read: Box.box(read_checked) }),
						Node.Attr.OnEvent({ kind: Node.event_kind_check, msg }),
					],
					attrs,
				),
				children: [],
			},
		)
	}
}
