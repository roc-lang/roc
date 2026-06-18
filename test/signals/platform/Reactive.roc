import NodeValue exposing [NodeValue]
import Graph

Reactive := [].{
	Unit := [UnitValue].{
		value : Unit
		value = UnitValue

		encode : Unit, NodeValue -> Try(NodeValue, [])
		encode = |_unit, fmt| fmt.encode_unit({})

		decode : NodeValue, NodeValue -> (Try(Unit, [TypeMismatch]), NodeValue)
		decode = |nv, fmt| {
			(result, rest) = NodeValue.decode_unit(fmt, nv)
			unit_result =
				match result {
					Ok(_) => Ok(Unit.value)
					Err(err) => Err(err)
				}

			(unit_result, rest)
		}
	}

	encode : a -> NodeValue where [a.encode : a, NodeValue -> Try(NodeValue, [])]
	encode = |value| {
		match value.encode(NodeValue.format) {
			Ok(nv) => nv
		}
	}

	EventSender(a) := { node : Graph.EventNode }.{
		to_node : EventSender(a) -> Graph.EventNode
		to_node = |sender| sender.node
	}

	Event(a) := { node : Graph.EventNode }.{
		to_node : Event(a) -> Graph.EventNode
		to_node = |event| event.node

		channel : Str, U64 -> { sender : EventSender(a), receiver : Event(a) }
		channel = |key, payload_kind| {
			event_node = Graph.EventNode.make_source(key, payload_kind)
			{
				sender: { node: event_node },
				receiver: { node: event_node },
			}
		}

		unit_channel : Str -> { sender : EventSender(Unit), receiver : Event(Unit) }
		unit_channel = |key| Event.channel(key, Graph.unit_payload_kind)

		str_channel : Str -> { sender : EventSender(Str), receiver : Event(Str) }
		str_channel = |key| Event.channel(key, Graph.str_payload_kind)

		bool_channel : Str -> { sender : EventSender(Bool), receiver : Event(Bool) }
		bool_channel = |key| Event.channel(key, Graph.bool_payload_kind)

		map :
			Event(a), (a -> b) -> Event(b)
				where [
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					b.encode : b, NodeValue -> Try(NodeValue, []),
				]
		map = |event, f| {
			source = event.node
			wrapped : NodeValue -> NodeValue
			wrapped = |input_nv| {
				A : a
				typed_input : a
				typed_input =
					match A.decode(input_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Event.map received a value that does not match the source event type"
						}
					}
				typed_output : b
				typed_output = f(typed_input)
				Reactive.encode(typed_output)
			}

			{ node: Graph.EventNode.make_map_event(source, Box.box(wrapped)) }
		}

		map_unit_i64_const : Event(Unit), I64 -> Event(I64)
		map_unit_i64_const = |event, value| {
			{ node: Graph.EventNode.make_map_unit_i64_const(event.node, value) }
		}

		merge : Event(a), Event(a) -> Event(a)
		merge = |left, right| {
			{ node: Graph.EventNode.make_merge(left.node, right.node) }
		}
	}

	Signal(a) := { node : Graph.SignalNode }.{
		to_node : Signal(a) -> Graph.SignalNode
		to_node = |signal| signal.node

		hold : Str, a, Event(a) -> Signal(a) where [a.encode : a, NodeValue -> Try(NodeValue, [])]
		hold = |key, initial, event| {
			initial_nv = Reactive.encode(initial)
			{ node: Graph.SignalNode.make_hold(key, initial_nv, Event.to_node(event)) }
		}

		const : a -> Signal(a) where [a.encode : a, NodeValue -> Try(NodeValue, [])]
		const = |value| {
			nv = Reactive.encode(value)
			{ node: Graph.SignalNode.make_const(nv) }
		}

		const_i64 : I64 -> Signal(I64)
		const_i64 = |value| {
			{ node: Graph.SignalNode.make_const_i64(value) }
		}

		const_bool : Bool -> Signal(Bool)
		const_bool = |value| {
			{ node: Graph.SignalNode.make_const_bool(value) }
		}

		const_str : Str -> Signal(Str)
		const_str = |value| {
			{ node: Graph.SignalNode.make_const_str(value) }
		}

		map :
			Signal(a), (a -> b) -> Signal(b)
				where [
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					b.encode : b, NodeValue -> Try(NodeValue, []),
				]
		map = |signal, f| {
			source = signal.node
			wrapped : NodeValue -> NodeValue
			wrapped = |input_nv| {
				A : a
				typed_input : a
				typed_input =
					match A.decode(input_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.map received a value that does not match the source signal type"
						}
					}
				typed_output : b
				typed_output = f(typed_input)
				Reactive.encode(typed_output)
			}

			{ node: Graph.SignalNode.make_map_signal(source, Box.box(wrapped)) }
		}

		map_keyed :
			Str, Signal(a), (a -> b) -> Signal(b)
				where [
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					b.encode : b, NodeValue -> Try(NodeValue, []),
				]
		map_keyed = |key, signal, f| {
			source = signal.node
			wrapped : NodeValue -> NodeValue
			wrapped = |input_nv| {
				A : a
				typed_input : a
				typed_input =
					match A.decode(input_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.map_keyed received a value that does not match the source signal type"
						}
					}
				typed_output : b
				typed_output = f(typed_input)
				Reactive.encode(typed_output)
			}

			{ node: Graph.SignalNode.make_keyed_map_signal(key, source, Box.box(wrapped)) }
		}

		map_i64_i64 : Signal(I64), (I64 -> I64) -> Signal(I64)
		map_i64_i64 = |signal, f| {
			{ node: Graph.SignalNode.make_map_i64_i64(signal.node, Box.box(f)) }
		}

		map_i64_i64_keyed : Str, Signal(I64), (I64 -> I64) -> Signal(I64)
		map_i64_i64_keyed = |key, signal, f| {
			{ node: Graph.SignalNode.make_keyed_map_i64_i64(key, signal.node, Box.box(f)) }
		}

		map_i64_str : Signal(I64), (I64 -> Str) -> Signal(Str)
		map_i64_str = |signal, f| {
			{ node: Graph.SignalNode.make_map_i64_str(signal.node, Box.box(f)) }
		}

		map_i64_str_keyed : Str, Signal(I64), (I64 -> Str) -> Signal(Str)
		map_i64_str_keyed = |key, signal, f| {
			{ node: Graph.SignalNode.make_keyed_map_i64_str(key, signal.node, Box.box(f)) }
		}

		map2 :
			Signal(a), Signal(b), (a, b -> c) -> Signal(c)
				where [
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					b.decode : NodeValue, NodeValue -> (Try(b, [TypeMismatch]), NodeValue),
					c.encode : c, NodeValue -> Try(NodeValue, []),
				]
		map2 = |left_signal, right_signal, f| {
			wrapped : (NodeValue, NodeValue) -> NodeValue
			wrapped = |(left_nv, right_nv)| {
				A : a
				left : a
				left =
					match A.decode(left_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.map2 received a value that does not match the left source signal type"
						}
					}
				B : b
				right : b
				right =
					match B.decode(right_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.map2 received a value that does not match the right source signal type"
						}
					}
				output : c
				output = f(left, right)
				Reactive.encode(output)
			}

			{
				node: Graph.SignalNode.make_map2_signal(
					left_signal.node,
					right_signal.node,
					Box.box(wrapped),
				),
			}
		}

		map2_keyed :
			Str, Signal(a), Signal(b), (a, b -> c) -> Signal(c)
				where [
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					b.decode : NodeValue, NodeValue -> (Try(b, [TypeMismatch]), NodeValue),
					c.encode : c, NodeValue -> Try(NodeValue, []),
				]
		map2_keyed = |key, left_signal, right_signal, f| {
			wrapped : (NodeValue, NodeValue) -> NodeValue
			wrapped = |(left_nv, right_nv)| {
				A : a
				left : a
				left =
					match A.decode(left_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.map2_keyed received a value that does not match the left source signal type"
						}
					}
				B : b
				right : b
				right =
					match B.decode(right_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.map2_keyed received a value that does not match the right source signal type"
						}
					}
				output : c
				output = f(left, right)
				Reactive.encode(output)
			}

			{
				node: Graph.SignalNode.make_keyed_map2_signal(
					key,
					left_signal.node,
					right_signal.node,
					Box.box(wrapped),
				),
			}
		}

		map2_i64_i64 : Signal(I64), Signal(I64), (I64, I64 -> I64) -> Signal(I64)
		map2_i64_i64 = |left_signal, right_signal, f| {
			wrapped : (I64, I64) -> I64
			wrapped = |(left, right)| f(left, right)

			{ node: Graph.SignalNode.make_map2_i64_i64(left_signal.node, right_signal.node, Box.box(wrapped)) }
		}

		map2_i64_i64_keyed : Str, Signal(I64), Signal(I64), (I64, I64 -> I64) -> Signal(I64)
		map2_i64_i64_keyed = |key, left_signal, right_signal, f| {
			wrapped : (I64, I64) -> I64
			wrapped = |(left, right)| f(left, right)

			{ node: Graph.SignalNode.make_keyed_map2_i64_i64(key, left_signal.node, right_signal.node, Box.box(wrapped)) }
		}

		map2_i64_i64_str : Signal(I64), Signal(I64), (I64, I64 -> Str) -> Signal(Str)
		map2_i64_i64_str = |left_signal, right_signal, f| {
			wrapped : (I64, I64) -> Str
			wrapped = |(left, right)| f(left, right)

			{ node: Graph.SignalNode.make_map2_i64_i64_str(left_signal.node, right_signal.node, Box.box(wrapped)) }
		}

		map2_i64_i64_str_keyed : Str, Signal(I64), Signal(I64), (I64, I64 -> Str) -> Signal(Str)
		map2_i64_i64_str_keyed = |key, left_signal, right_signal, f| {
			wrapped : (I64, I64) -> Str
			wrapped = |(left, right)| f(left, right)

			{ node: Graph.SignalNode.make_keyed_map2_i64_i64_str(key, left_signal.node, right_signal.node, Box.box(wrapped)) }
		}

		fold :
			Str, a, Event(e), (a, e -> a) -> Signal(a)
				where [
					a.encode : a, NodeValue -> Try(NodeValue, []),
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					e.decode : NodeValue, NodeValue -> (Try(e, [TypeMismatch]), NodeValue),
				]
		fold = |key, initial, event, step_fn| {
			initial_nv = Reactive.encode(initial)
			wrapped : (NodeValue, NodeValue) -> NodeValue
			wrapped = |(acc_nv, evt_nv)| {
				A : a
				acc : a
				acc =
					match A.decode(acc_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.fold received an accumulator value that does not match the signal type"
						}
					}
				E : e
				evt : e
				evt =
					match E.decode(evt_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Reactive.Signal.fold received an event value that does not match the event type"
						}
					}
				new_acc : a
				new_acc = step_fn(acc, evt)
				Reactive.encode(new_acc)
			}

			{ node: Graph.SignalNode.make_fold(key, initial_nv, Event.to_node(event), Box.box(wrapped)) }
		}

		fold_i64 : Str, I64, Event(I64), (I64, I64 -> I64) -> Signal(I64)
		fold_i64 = |key, initial, event, step_fn| {
			wrapped : (I64, I64) -> I64
			wrapped = |(current, delta)| step_fn(current, delta)

			{ node: Graph.SignalNode.make_fold_i64(key, initial, Event.to_node(event), Box.box(wrapped)) }
		}

		fold_bool_toggle : Str, Bool, Event(Unit) -> Signal(Bool)
		fold_bool_toggle = |key, initial, event| {
			{ node: Graph.SignalNode.make_fold_bool_toggle(key, initial, Event.to_node(event)) }
		}
	}
}
