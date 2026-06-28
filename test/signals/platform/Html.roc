import Elem exposing [Elem]
import HostValue exposing [HostValue]
import Capability exposing [Capability]
import Node
import Signal exposing [Signal]

## Static UI structure and attributes. Markup carries no identity; dynamic text
## and attributes reference signals, and event handlers carry reducer messages.
Html := [].{
	Attr : Node.Attr

	class_attr : Str -> Node.Attr
	class_attr = |value| Node.Attr.StaticText({ field: Node.field_class, name: "", value })

	class_attr_s : Signal(Str) -> Node.Attr
	class_attr_s = |signal| {
		cap = signal.cap
		read : HostValue -> Str
		read = |value| Box.unbox(Capability.get(value, cap))
		Node.Attr.SignalText({ field: Node.field_class, name: "", signal: Signal.to_expr(signal), read: { capability: Capability.handle(cap), read: Box.box(read) } })
	}

	attr : Str, Str -> Node.Attr
	attr = |name, value| Node.Attr.StaticText({ field: Node.field_custom, name, value })

	attr_s : Str, Signal(Str) -> Node.Attr
	attr_s = |name, signal| {
		cap = signal.cap
		read : HostValue -> Str
		read = |value| Box.unbox(Capability.get(value, cap))
		Node.Attr.SignalText({ field: Node.field_custom, name, signal: Signal.to_expr(signal), read: { capability: Capability.handle(cap), read: Box.box(read) } })
	}

	on_pointer_down : Node.Msg -> Node.Attr
	on_pointer_down = |msg| Node.Attr.OnEvent({ kind: Node.event_kind_pointer_down, msg })

	on_pointer_up : Node.Msg -> Node.Attr
	on_pointer_up = |msg| Node.Attr.OnEvent({ kind: Node.event_kind_pointer_up, msg })

	on_pointer_enter : Node.Msg -> Node.Attr
	on_pointer_enter = |msg| Node.Attr.OnEvent({ kind: Node.event_kind_pointer_enter, msg })

	on_pointer_leave : Node.Msg -> Node.Attr
	on_pointer_leave = |msg| Node.Attr.OnEvent({ kind: Node.event_kind_pointer_leave, msg })

	on_event : Str, U64, Node.Msg -> Node.Attr
	on_event = |name, options, msg| Node.Attr.OnNamedEvent({ name, options, msg })

	on_key_down : Node.Msg -> Node.Attr
	on_key_down = |msg| on_event("keydown", 0, msg)

	on_submit_prevent_default : Node.Msg -> Node.Attr
	on_submit_prevent_default = |msg| on_event("submit", Node.listener_prevent_default, msg)

	div : List(Node.Attr), List(Elem) -> Elem
	div = |attrs, children| Elem.Element({ tag: "div", attrs, children })

	form : List(Node.Attr), List(Elem) -> Elem
	form = |attrs, children| Elem.Element({ tag: "form", attrs, children })

	form_label : Str, List(Node.Attr), List(Elem) -> Elem
	form_label = |label, attrs, children| {
		base = [
			Node.Attr.StaticText({ field: Node.field_role, name: "", value: "form" }),
			Node.Attr.StaticText({ field: Node.field_label, name: "", value: label }),
		]
		form(List.concat(base, attrs), children)
	}

	link : Str, List(Node.Attr) -> Elem
	link = |label, attrs| {
		base = [
			Node.Attr.StaticText({ field: Node.field_role, name: "", value: "link" }),
			Node.Attr.StaticText({ field: Node.field_label, name: "", value: label }),
			Node.Attr.StaticText({ field: Node.field_text, name: "", value: label }),
		]
		Elem.Element({ tag: "a", attrs: List.concat(base, attrs), children: [] })
	}

	div_c : Str, List(Elem) -> Elem
	div_c = |classes, children| div([class_attr(classes)], children)

	div_sc : Signal(Str), List(Elem) -> Elem
	div_sc = |classes, children| div([class_attr_s(classes)], children)

	section : Str, List(Node.Attr), List(Elem) -> Elem
	section = |label, attrs, children| {
		base = [
			Node.Attr.StaticText({ field: Node.field_role, name: "", value: "region" }),
			Node.Attr.StaticText({ field: Node.field_label, name: "", value: label }),
		]
		Elem.Element({ tag: "section", attrs: List.concat(base, attrs), children })
	}

	section_c : Str, Str, List(Elem) -> Elem
	section_c = |label, classes, children| section(label, [class_attr(classes)], children)

	section_sc : Str, Signal(Str), List(Elem) -> Elem
	section_sc = |label, classes, children| section(label, [class_attr_s(classes)], children)

	heading : Str -> Elem
	heading = |text_value| {
		Elem.Element(
			{
				tag: "h2",
				attrs: [
					Node.Attr.StaticText({ field: Node.field_role, name: "", value: "heading" }),
					Node.Attr.StaticText({ field: Node.field_text, name: "", value: text_value }),
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
					Node.Attr.StaticText({ field: Node.field_role, name: "", value: "heading" }),
					Node.Attr.StaticText({ field: Node.field_text, name: "", value: text_value }),
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
				attrs: [Node.Attr.StaticText({ field: Node.field_text, name: "", value: text_value })],
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
					Node.Attr.StaticText({ field: Node.field_text, name: "", value: text_value }),
					class_attr(classes),
				],
				children: [],
			},
		)
	}

	paragraph_s : Signal(Str) -> Elem
	paragraph_s = |signal| paragraph_s_c(signal, "")

	paragraph_s_c : Signal(Str), Str -> Elem
	paragraph_s_c = |signal, classes| {
		cap = signal.cap
		read : HostValue -> Str
		read = |value| Box.unbox(Capability.get(value, cap))
		Elem.Element(
			{
				tag: "p",
				attrs: [
					Node.Attr.SignalText({ field: Node.field_text, name: "", signal: Signal.to_expr(signal), read: { capability: Capability.handle(cap), read: Box.box(read) } }),
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
		cap = signal.cap
		read : HostValue -> Str
		read = |value| Box.unbox(Capability.get(value, cap))
		Elem.TextSignal({ signal: Signal.to_expr(signal), read: { capability: Capability.handle(cap), read: Box.box(read) } })
	}

	## Signal-backed preformatted text block.
	pre_s_c : Signal(Str), Str -> Elem
	pre_s_c = |signal, classes| {
		cap = signal.cap
		read : HostValue -> Str
		read = |value| Box.unbox(Capability.get(value, cap))
		Elem.Element(
			{
				tag: "pre",
				attrs: [
					Node.Attr.SignalText({ field: Node.field_text, name: "", signal: Signal.to_expr(signal), read: { capability: Capability.handle(cap), read: Box.box(read) } }),
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
						Node.Attr.StaticText({ field: Node.field_text, name: "", value: label }),
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
		label_cap = label.cap
		read_label : HostValue -> Str
		read_label = |value| Box.unbox(Capability.get(value, label_cap))
		Elem.Element(
			{
				tag: "button",
				attrs: List.concat(
					[
						Node.Attr.SignalText({ field: Node.field_text, name: "", signal: Signal.to_expr(label), read: { capability: Capability.handle(label_cap), read: Box.box(read_label) } }),
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
		label_cap = label.cap
		disabled_cap = disabled.cap
		read_label : HostValue -> Str
		read_label = |value| Box.unbox(Capability.get(value, label_cap))
		read_disabled : HostValue -> Bool
		read_disabled = |value| Box.unbox(Capability.get(value, disabled_cap))
		Elem.Element(
			{
				tag: "button",
				attrs: List.concat(
					[
						Node.Attr.SignalText({ field: Node.field_text, name: "", signal: Signal.to_expr(label), read: { capability: Capability.handle(label_cap), read: Box.box(read_label) } }),
						Node.Attr.SignalBool({ field: Node.bool_field_disabled, signal: Signal.to_expr(disabled), read: { capability: Capability.handle(disabled_cap), read: Box.box(read_disabled) } }),
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
		value_cap = value.cap
		read_value : HostValue -> Str
		read_value = |host_value| Box.unbox(Capability.get(host_value, value_cap))
		Elem.Element(
			{
				tag: "input",
				attrs: List.concat(
					[
						Node.Attr.StaticText({ field: Node.field_role, name: "", value: "textbox" }),
						Node.Attr.StaticText({ field: Node.field_label, name: "", value: label }),
						Node.Attr.SignalText({ field: Node.field_value, name: "", signal: Signal.to_expr(value), read: { capability: Capability.handle(value_cap), read: Box.box(read_value) } }),
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
		checked_cap = checked.cap
		read_checked : HostValue -> Bool
		read_checked = |value| Box.unbox(Capability.get(value, checked_cap))
		Elem.Element(
			{
				tag: "input",
				attrs: List.concat(
					[
						Node.Attr.StaticText({ field: Node.field_role, name: "", value: "checkbox" }),
						Node.Attr.StaticText({ field: Node.field_label, name: "", value: label }),
						Node.Attr.SignalBool({ field: Node.bool_field_checked, signal: Signal.to_expr(checked), read: { capability: Capability.handle(checked_cap), read: Box.box(read_checked) } }),
						Node.Attr.OnEvent({ kind: Node.event_kind_check, msg }),
					],
					attrs,
				),
				children: [],
			},
		)
	}
}
