import Reactive
import Graph
import NodeValue exposing [NodeValue]

## Pure UI element tree. The Roc runtime renders this tree into host commands.
Elem := [
	Div(List(Elem)),
	Button({ on_click : Graph.EventNode, label : Graph.SignalNode }),
	ActionButton({ on_click : Graph.EventNode, label : Graph.SignalNode, disabled : Graph.SignalNode }),
	Label({ signal : Graph.SignalNode }),
	Text(Str),
	Heading(Str),
	Paragraph(Str),
	Section({ label : Str, children : List(Elem) }),
	TextInput({ label : Str, value : Graph.SignalNode, on_input : Graph.EventNode, disabled : Graph.SignalNode }),
	Checkbox({ label : Str, checked : Graph.SignalNode, on_check : Graph.EventNode, disabled : Graph.SignalNode }),
	Dynamic({ signal : Graph.SignalNode, render : Box((NodeValue -> Elem)) }),
	DynamicKeyed({ signal : Graph.SignalNode, key : Box((NodeValue -> Str)), render : Box((NodeValue -> Elem)) }),
	Each({ signal : Graph.SignalNode, key : Box((NodeValue -> Str)), render : Box((NodeValue -> Elem)) }),
].{
	div : List(Elem) -> Elem
	div = |children| Div(children)

	button : { on_click : Reactive.EventSender(Reactive.Unit), label : Reactive.Signal(Str) } -> Elem
	button = |config| {
		Button(
			{
				on_click: Reactive.EventSender.to_node(config.on_click),
				label: Reactive.Signal.to_node(config.label),
			},
		)
	}

	action_button :
		{
			on_click : Reactive.EventSender(Reactive.Unit),
			label : Reactive.Signal(Str),
			disabled : Reactive.Signal(Bool),
		} -> Elem
	action_button = |config| {
		ActionButton(
			{
				on_click: Reactive.EventSender.to_node(config.on_click),
				label: Reactive.Signal.to_node(config.label),
				disabled: Reactive.Signal.to_node(config.disabled),
			},
		)
	}

	label : Reactive.Signal(Str) -> Elem
	label = |text_signal| Label({ signal: Reactive.Signal.to_node(text_signal) })

	text : Str -> Elem
	text = |s| Text(s)

	heading : Str -> Elem
	heading = |s| Heading(s)

	paragraph : Str -> Elem
	paragraph = |s| Paragraph(s)

	section : Str, List(Elem) -> Elem
	section = |section_label, children| Section({ label: section_label, children })

	text_input :
		{
			label : Str,
			value : Reactive.Signal(Str),
			on_input : Reactive.EventSender(Str),
			disabled : Reactive.Signal(Bool),
		} -> Elem
	text_input = |config| {
		TextInput(
			{
				label: config.label,
				value: Reactive.Signal.to_node(config.value),
				on_input: Reactive.EventSender.to_node(config.on_input),
				disabled: Reactive.Signal.to_node(config.disabled),
			},
		)
	}

	checkbox :
		{
			label : Str,
			checked : Reactive.Signal(Bool),
			on_check : Reactive.EventSender(Bool),
			disabled : Reactive.Signal(Bool),
		} -> Elem
	checkbox = |config| {
		Checkbox(
			{
				label: config.label,
				checked: Reactive.Signal.to_node(config.checked),
				on_check: Reactive.EventSender.to_node(config.on_check),
				disabled: Reactive.Signal.to_node(config.disabled),
			},
		)
	}

	dynamic :
		Reactive.Signal(a), (a -> Elem) -> Elem
			where [a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue)]
	dynamic = |signal, render| {
		wrapped : NodeValue -> Elem
		wrapped = |nv| {
			A : a
			value : a
			value =
				match A.decode(nv, NodeValue.format) {
					(Ok(val), _) => val
					(Err(_), _) => ...
				}

			render(value)
		}

		Dynamic(
			{
				signal: Reactive.Signal.to_node(signal),
				render: Box.box(wrapped),
			},
		)
	}

	when : Reactive.Signal(Bool), Elem, Elem -> Elem
	when = |condition, when_true, when_false| {
		Elem.dynamic(
			condition,
			|flag| if flag {
				when_true
			} else {
				when_false
			},
		)
	}

	dynamic_keyed :
		Reactive.Signal(a), (a -> Str), (a -> Elem) -> Elem
			where [a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue)]
	dynamic_keyed = |signal, key_fn, render| {
		wrapped_key : NodeValue -> Str
		wrapped_key = |nv| {
			A : a
			value : a
			value =
				match A.decode(nv, NodeValue.format) {
					(Ok(val), _) => val
					(Err(_), _) => ...
				}

			key_fn(value)
		}

		wrapped_render : NodeValue -> Elem
		wrapped_render = |nv| {
			A : a
			value : a
			value =
				match A.decode(nv, NodeValue.format) {
					(Ok(val), _) => val
					(Err(_), _) => ...
				}

			render(value)
		}

		DynamicKeyed(
			{
				signal: Reactive.Signal.to_node(signal),
				key: Box.box(wrapped_key),
				render: Box.box(wrapped_render),
			},
		)
	}

	each :
		Reactive.Signal(List(a)), (a -> Str), (a -> Elem) -> Elem
			where [a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue)]
	each = |items_signal, key_fn, render| {
		wrapped_key : NodeValue -> Str
		wrapped_key = |nv| {
			A : a
			value : a
			value =
				match A.decode(nv, NodeValue.format) {
					(Ok(val), _) => val
					(Err(_), _) => ...
				}

			key_fn(value)
		}

		wrapped_render : NodeValue -> Elem
		wrapped_render = |nv| {
			A : a
			value : a
			value =
				match A.decode(nv, NodeValue.format) {
					(Ok(val), _) => val
					(Err(_), _) => ...
				}

			render(value)
		}

		Each(
			{
				signal: Reactive.Signal.to_node(items_signal),
				key: Box.box(wrapped_key),
				render: Box.box(wrapped_render),
			},
		)
	}
}
