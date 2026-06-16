import NodeValue exposing [NodeValue]
import Graph
import Host

## Public reactive types. Signals and events carry typed graph nodes. Values cross
## the host boundary through NodeValue using Roc's static-dispatch encode/decode
## methods instead of explicit runtime codec records.
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

		send! : EventSender(a), a => {} where [a.encode : a, NodeValue -> Try(NodeValue, [])]
		send! = |sender, value| {
			event_id = Graph.EventNode.walk!(sender.node)
			Host.send_event!(event_id, Reactive.encode(value))
		}
	}

	Event(a) := { node : Graph.EventNode }.{
		to_node : Event(a) -> Graph.EventNode
		to_node = |event| event.node

		from_node : Graph.EventNode -> Event(a)
		from_node = |node| { node: node }

		channel! : () => { sender : EventSender(a), receiver : Event(a) }
		channel! = || {
			host_id = Host.create_event_source!()
			event_node = Graph.EventNode.make_prebuilt(host_id)
			{
				sender: { node: event_node },
				receiver: { node: event_node },
			}
		}

		unit_channel! : () => { sender : EventSender(Unit), receiver : Event(Unit) }
		unit_channel! = || Event.channel!()

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
						(Err(_), _) => ...
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

		with_latest :
			Event(e), Signal(s), (e, s -> out) -> Event(out)
				where [
					e.decode : NodeValue, NodeValue -> (Try(e, [TypeMismatch]), NodeValue),
					s.decode : NodeValue, NodeValue -> (Try(s, [TypeMismatch]), NodeValue),
					out.encode : out, NodeValue -> Try(NodeValue, []),
				]
		with_latest = |event, signal, combine| {
			wrapped : (NodeValue, NodeValue) -> NodeValue
			wrapped = |(event_nv, signal_nv)| {
				E : e
				typed_event : e
				typed_event = 
					match E.decode(event_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
					}
				S : s
				typed_signal : s
				typed_signal = 
					match S.decode(signal_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
					}
				output : out
				output = combine(typed_event, typed_signal)
				Reactive.encode(output)
			}

			{
				node: Graph.EventNode.make_with_latest(
					event.node,
					signal.node,
					Box.box(wrapped),
				),
			}
		}

		with_latest_unit :
			Event(Unit), Signal(s), (s -> out) -> Event(out)
				where [
					s.decode : NodeValue, NodeValue -> (Try(s, [TypeMismatch]), NodeValue),
					out.encode : out, NodeValue -> Try(NodeValue, []),
				]
		with_latest_unit = |event, signal, combine| {
			wrapped : (NodeValue, NodeValue) -> NodeValue
			wrapped = |(_event_nv, signal_nv)| {
				S : s
				typed_signal : s
				typed_signal = 
					match S.decode(signal_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
					}
				output : out
				output = combine(typed_signal)
				Reactive.encode(output)
			}

			{
				node: Graph.EventNode.make_with_latest(
					event.node,
					signal.node,
					Box.box(wrapped),
				),
			}
		}
	}

	Signal(a) := { node : Graph.SignalNode }.{
		to_node : Signal(a) -> Graph.SignalNode
		to_node = |signal| signal.node

		from_node : Graph.SignalNode -> Signal(a)
		from_node = |node| { node: node }

		state! : a => Signal(a) where [a.encode : a, NodeValue -> Try(NodeValue, [])]
		state! = |value| {
			nv = Reactive.encode(value)
			host_id = Host.create_signal_state!(nv)
			{ node: Graph.SignalNode.make_prebuilt_signal(host_id) }
		}

		hold! : a, Event(a) => Signal(a) where [a.encode : a, NodeValue -> Try(NodeValue, [])]
		hold! = |initial, event| {
			initial_nv = Reactive.encode(initial)
			event_id = Graph.EventNode.walk!(Event.to_node(event))
			host_id = Host.create_signal_hold!(initial_nv, event_id)
			{ node: Graph.SignalNode.make_prebuilt_signal(host_id) }
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
						(Err(_), _) => ...
					}
				typed_output : b
				typed_output = f(typed_input)
				Reactive.encode(typed_output)
			}

			{ node: Graph.SignalNode.make_map_signal(source, Box.box(wrapped)) }
		}

		map_i64_i64 : Signal(I64), (I64 -> I64) -> Signal(I64)
		map_i64_i64 = |signal, f| {
			{ node: Graph.SignalNode.make_map_i64_i64(signal.node, Box.box(f)) }
		}

		map_i64_str : Signal(I64), (I64 -> Str) -> Signal(Str)
		map_i64_str = |signal, f| {
			{ node: Graph.SignalNode.make_map_i64_str(signal.node, Box.box(f)) }
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
						(Err(_), _) => ...
					}
				B : b
				right : b
				right = 
					match B.decode(right_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
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

		map2_i64_i64 : Signal(I64), Signal(I64), (I64, I64 -> I64) -> Signal(I64)
		map2_i64_i64 = |left_signal, right_signal, f| {
			wrapped : (I64, I64) -> I64
			wrapped = |(left, right)| f(left, right)

			{ node: Graph.SignalNode.make_map2_i64_i64(left_signal.node, right_signal.node, Box.box(wrapped)) }
		}

		map2_i64_i64_str : Signal(I64), Signal(I64), (I64, I64 -> Str) -> Signal(Str)
		map2_i64_i64_str = |left_signal, right_signal, f| {
			wrapped : (I64, I64) -> Str
			wrapped = |(left, right)| f(left, right)

			{ node: Graph.SignalNode.make_map2_i64_i64_str(left_signal.node, right_signal.node, Box.box(wrapped)) }
		}

		fold! :
			a, Event(e), (a, e -> a) => Signal(a)
				where [
					a.encode : a, NodeValue -> Try(NodeValue, []),
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					e.decode : NodeValue, NodeValue -> (Try(e, [TypeMismatch]), NodeValue),
				]
		fold! = |initial, event, step_fn| {
			initial_nv = Reactive.encode(initial)
			event_id = Graph.EventNode.walk!(Event.to_node(event))
			wrapped : (NodeValue, NodeValue) -> NodeValue
			wrapped = |(acc_nv, evt_nv)| {
				A : a
				acc : a
				acc = 
					match A.decode(acc_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
					}
				E : e
				evt : e
				evt = 
					match E.decode(evt_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
					}
				new_acc : a
				new_acc = step_fn(acc, evt)
				Reactive.encode(new_acc)
			}

			host_id = Host.create_signal_fold!(initial_nv, event_id, Box.box(wrapped))
			{ node: Graph.SignalNode.make_prebuilt_signal(host_id) }
		}

		fold_i64! : I64, Event(I64), (I64, I64 -> I64) => Signal(I64)
		fold_i64! = |initial, event, step_fn| {
			wrapped : (I64, I64) -> I64
			wrapped = |(current, delta)| step_fn(current, delta)

			event_id = Graph.EventNode.walk!(Event.to_node(event))
			host_id = Host.create_signal_fold_i64!(initial, event_id, Box.box(wrapped))
			{ node: Graph.SignalNode.make_prebuilt_signal(host_id) }
		}

		fold_bool_toggle! : Bool, Event(Unit) => Signal(Bool)
		fold_bool_toggle! = |initial, event| {
			event_id = Graph.EventNode.walk!(Event.to_node(event))
			host_id = Host.create_signal_fold_bool_toggle!(initial, event_id)
			{ node: Graph.SignalNode.make_prebuilt_signal(host_id) }
		}

		zip_with! :
			Signal(a), Event(e), (a, e -> a) => Signal(a)
				where [
					a.encode : a, NodeValue -> Try(NodeValue, []),
					a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
					e.decode : NodeValue, NodeValue -> (Try(e, [TypeMismatch]), NodeValue),
				]
		zip_with! = |signal, event, combine| {
			wrapped : (NodeValue, NodeValue) -> NodeValue
			wrapped = |(signal_nv, event_nv)| {
				A : a
				typed_signal : a
				typed_signal = 
					match A.decode(signal_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
					}
				E : e
				typed_event : e
				typed_event = 
					match E.decode(event_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => ...
					}
				output : a
				output = combine(typed_signal, typed_event)
				Reactive.encode(output)
			}

			source_id = Graph.SignalNode.walk!(signal.node)
			event_id = Graph.EventNode.walk!(Event.to_node(event))
			host_id = Host.create_signal_zip_with!(source_id, event_id, Box.box(wrapped))
			{ node: Graph.SignalNode.make_prebuilt_signal(host_id) }
		}
	}
}
