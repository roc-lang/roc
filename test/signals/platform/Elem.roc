import Reactive
import Graph
import Host
import NodeValue exposing [NodeValue]

## UI Element tree containing signal and event graphs.
Elem := [
	Div(List(Elem)),
	Button({ on_click : Graph.EventNode, label : Graph.SignalNode }),
	Label({ signal : Graph.SignalNode }),
	Text(Str),
	Dynamic({ signal : Graph.SignalNode, render : Box(((NodeValue, U64) => {})) }),
	Each({ signal : Graph.SignalNode, key : Box((NodeValue -> Str)), render : Box(((NodeValue, U64) => {})) }),
	Lifecycle({ on_mount : Graph.EventNode, on_unmount : Graph.EventNode }),
].{

	## Hosted effects with effectful boxed render callbacks. The callbacks
	## mount Elem trees from Roc, keeping Elem itself out of the host ABI.
	create_dynamic! : U64, U64, Box(((NodeValue, U64) => {})) => {}

	create_each! : U64, U64, Box((NodeValue -> Str)), Box(((NodeValue, U64) => {})) => {}

	register_lifecycle! : U64, U64 => {}

	Component(a) := { elem : Elem, changes : Reactive.Event(a) }.{
		elem : Component(a) -> Elem
		elem = |component| component.elem

		changes : Component(a) -> Reactive.Event(a)
		changes = |component| component.changes
	}

	walk! : Elem, U64 => {}
	walk! = |elem, parent_id| {
		match elem {
			Div(children) => {
				div_id = Host.create_element!("div")
				Host.append_child!(parent_id, div_id)
				List.for_each!(
					children,
					|child| {
						Elem.walk!(child, div_id)
					},
				)
			}

			Button({ on_click, label: btn_label }) => {
				btn_id = Host.create_element!("button")
				Host.append_child!(parent_id, btn_id)

				label_node_id = Graph.SignalNode.walk!(btn_label)
				Host.bind_text!(btn_id, label_node_id)

				click_node_id = Graph.EventNode.walk!(on_click)
				Host.bind_click!(btn_id, click_node_id)
			}

			Label({ signal: text_signal }) => {
				span_id = Host.create_element!("span")
				Host.append_child!(parent_id, span_id)

				text_node_id = Graph.SignalNode.walk!(text_signal)
				Host.bind_text!(span_id, text_node_id)
			}

			Text(s) => {
				span_id = Host.create_element!("span")
				Host.set_text!(span_id, s)
				Host.append_child!(parent_id, span_id)
			}

			Dynamic({ signal, render }) => {
				container_id = Host.create_element!("div")
				Host.append_child!(parent_id, container_id)

				signal_id = Graph.SignalNode.walk!(signal)
				Elem.create_dynamic!(container_id, signal_id, render)
			}

			Each({ signal, key, render }) => {
				container_id = Host.create_element!("div")
				Host.append_child!(parent_id, container_id)

				signal_id = Graph.SignalNode.walk!(signal)
				Elem.create_each!(container_id, signal_id, key, render)
			}

			Lifecycle({ on_mount, on_unmount }) => {
				on_mount_id = Graph.EventNode.walk!(on_mount)
				on_unmount_id = Graph.EventNode.walk!(on_unmount)
				Elem.register_lifecycle!(on_mount_id, on_unmount_id)
			}
		}
	}

	run! : Elem => {}
	run! = |elem| {
		root = Host.create_root!()
		Elem.walk!(elem, root)
	}

	run_component! : a, (Reactive.Signal(a) => Component(a)) => {} where [a.encode : a, NodeValue -> Try(NodeValue, [])]
	run_component! = |initial, render!| {
		state = Reactive.Signal.state!(initial)
		component = render!(state)

		root = Host.create_root!()
		Elem.walk!(Component.elem(component), root)

		state_id = Graph.SignalNode.walk!(Reactive.Signal.to_node(state))
		changes_id = Graph.EventNode.walk!(Reactive.Event.to_node(Component.changes(component)))
		Host.bind_signal_update!(state_id, changes_id)
	}

	component : Elem, Reactive.Event(a) -> Component(a)
	component = |elem, changes| { elem: elem, changes: changes }

	translate :
		(Reactive.Signal(child) => Component(child)),
		(parent -> child),
		(parent, child -> parent) -> (Reactive.Signal(parent) => Component(parent))
			where [
				parent.encode : parent, NodeValue -> Try(NodeValue, []),
				parent.decode : NodeValue, NodeValue -> (Try(parent, [TypeMismatch]), NodeValue),
				child.encode : child, NodeValue -> Try(NodeValue, []),
				child.decode : NodeValue, NodeValue -> (Try(child, [TypeMismatch]), NodeValue),
			]
	translate = |child_render!, getter, setter| {
		|parent_signal| {
			child_signal = Reactive.Signal.map(parent_signal, getter)
			child_component = child_render!(child_signal)
			parent_changes =
				Reactive.Event.with_latest(
					Component.changes(child_component),
					parent_signal,
					|child, parent| setter(parent, child),
				)

			Elem.component(Component.elem(child_component), parent_changes)
		}
	}

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

	label : Reactive.Signal(Str) -> Elem
	label = |text_signal| Label({ signal: Reactive.Signal.to_node(text_signal) })

	text : Str -> Elem
	text = |s| Text(s)

	dynamic :
		Reactive.Signal(a), (a => Elem) -> Elem
			where [a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue)]
	dynamic = |signal, render| {
		wrapped : (NodeValue, U64) => {}
		wrapped = |(nv, parent_id)| {
			A : a
			value : a
			value =
				match A.decode(nv, NodeValue.format) {
					(Ok(val), _) => val
					(Err(_), _) => ...
				}

			Elem.walk!(render(value), parent_id)
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

	each :
		Reactive.Signal(List(a)), (a -> Str), (a => Elem) -> Elem
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

		wrapped_render : (NodeValue, U64) => {}
		wrapped_render = |(nv, parent_id)| {
			A : a
			value : a
			value =
				match A.decode(nv, NodeValue.format) {
					(Ok(val), _) => val
					(Err(_), _) => ...
				}

			Elem.walk!(render(value), parent_id)
		}

		Each(
			{
				signal: Reactive.Signal.to_node(items_signal),
				key: Box.box(wrapped_key),
				render: Box.box(wrapped_render),
			},
		)
	}

	lifecycle : { on_mount : Reactive.EventSender(Reactive.Unit), on_unmount : Reactive.EventSender(Reactive.Unit) } -> Elem
	lifecycle = |callbacks| {
		Lifecycle(
			{
				on_mount: Reactive.EventSender.to_node(callbacks.on_mount),
				on_unmount: Reactive.EventSender.to_node(callbacks.on_unmount),
			},
		)
	}

}
