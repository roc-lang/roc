import Elem exposing [Elem]
import HostValue exposing [HostValue]
import Node
import Signal exposing [Signal]

## Dynamic structure and local state. State is introduced through an explicit
## closure binder (`Ui.state`): the binder is the construction site, which is the
## only way to give per-instance state a stable identity in pure Roc. The host
## assigns construction-order identity by walking the descriptor tree; binders are
## referenced by their scoped token, so the same helper composes correctly
## wherever it is mounted.
Ui := [].{

	str_key_hash : Str -> U64
	str_key_hash = |value| {
		List.fold_with_index(
			Str.to_utf8(value),
			Str.count_utf8_bytes(value),
			|hash, byte, index| U64.bitwise_xor(U64.shift_left_by(hash, 5), U8.to_u64(byte) + index),
		)
	}

	## A handle to a state binder, given to the `Ui.state` body. `signal` reads the
	## current value; `send` builds a `Node.Msg` that, when its event fires, applies
	## the given reducer to the current value.
	State(a) := { ref : Node.BinderRef, tag : Box(HostValue.TypeTag(a)) }.{
		signal : State(a) -> Signal(a)
		signal = |st| Signal.from_expr(Node.SignalExpr.Ref(st.ref), st.tag)

		## Build a unit-triggered reducer message: `f` maps the current value to the
		## next value, ignoring the unit payload.
		on_unit : State(a), (a -> a) -> Node.Msg
		on_unit = |st, f| {
			current_tag = st.tag
			payload_unit_tag : Box(HostValue.TypeTag({}))
			payload_unit_tag = HostValue.new_unit_payload_tag({})
			payload_str_tag : Box(HostValue.TypeTag(Str))
			payload_str_tag = HostValue.new_str_payload_tag({})
			payload_bool_tag : Box(HostValue.TypeTag(Bool))
			payload_bool_tag = HostValue.new_bool_payload_tag({})
			wrapped : HostValue, HostValue -> HostValue
			wrapped = |current_hv, _payload_hv| {
				current : a
				current = Box.unbox(HostValue.get_tagged(current_hv, current_tag))
				next : a
				next = f(current)
				HostValue.store_tagged(Box.box(next), current_tag)
			}
			payload_drop : HostValue -> {}
			payload_drop = |payload_hv| {
				boxed : Box({})
				boxed = HostValue.take(payload_hv)
				_ = boxed
				{}
			}
			{ binder: st.ref, payload_kind: Node.unit_payload_kind, payload_unit_tag, payload_str_tag, payload_bool_tag, payload_drop: Box.box(payload_drop), transform: Box.box(wrapped) }
		}

		on_str : State(a), (a, Str -> a) -> Node.Msg
		on_str = |st, f| {
			current_tag = st.tag
			payload_unit_tag : Box(HostValue.TypeTag({}))
			payload_unit_tag = HostValue.new_unit_payload_tag({})
			payload_str_tag : Box(HostValue.TypeTag(Str))
			payload_str_tag = HostValue.new_str_payload_tag({})
			payload_bool_tag : Box(HostValue.TypeTag(Bool))
			payload_bool_tag = HostValue.new_bool_payload_tag({})
			wrapped : HostValue, HostValue -> HostValue
			wrapped = |current_hv, payload_hv| {
				current : a
				current = Box.unbox(HostValue.get_tagged(current_hv, current_tag))
				payload : Str
				payload = Box.unbox(HostValue.get_tagged(payload_hv, payload_str_tag))
				next : a
				next = f(current, payload)
				HostValue.store_tagged(Box.box(next), current_tag)
			}
			payload_drop : HostValue -> {}
			payload_drop = |payload_hv| {
				boxed : Box(Str)
				boxed = HostValue.take(payload_hv)
				_ = boxed
				{}
			}
			{ binder: st.ref, payload_kind: Node.str_payload_kind, payload_unit_tag, payload_str_tag, payload_bool_tag, payload_drop: Box.box(payload_drop), transform: Box.box(wrapped) }
		}

		on_bool : State(a), (a, Bool -> a) -> Node.Msg
		on_bool = |st, f| {
			current_tag = st.tag
			payload_unit_tag : Box(HostValue.TypeTag({}))
			payload_unit_tag = HostValue.new_unit_payload_tag({})
			payload_str_tag : Box(HostValue.TypeTag(Str))
			payload_str_tag = HostValue.new_str_payload_tag({})
			payload_bool_tag : Box(HostValue.TypeTag(Bool))
			payload_bool_tag = HostValue.new_bool_payload_tag({})
			wrapped : HostValue, HostValue -> HostValue
			wrapped = |current_hv, payload_hv| {
				current : a
				current = Box.unbox(HostValue.get_tagged(current_hv, current_tag))
				payload : Bool
				payload = Box.unbox(HostValue.get_tagged(payload_hv, payload_bool_tag))
				next : a
				next = f(current, payload)
				HostValue.store_tagged(Box.box(next), current_tag)
			}
			payload_drop : HostValue -> {}
			payload_drop = |payload_hv| {
				boxed : Box(Bool)
				boxed = HostValue.take(payload_hv)
				_ = boxed
				{}
			}
			{ binder: st.ref, payload_kind: Node.bool_payload_kind, payload_unit_tag, payload_str_tag, payload_bool_tag, payload_drop: Box.box(payload_drop), transform: Box.box(wrapped) }
		}
	}

	## Introduce a state binder. `init` is the initial value; `body` receives a
	## `State(a)` handle and returns the subtree built with that state in scope.
	## The host mints this binder's identity by its construction-order position.
	state :
		a, (State(a) -> Elem) -> Elem
			where [
				a.is_eq : a, a -> Bool,
			]
	state = |init, body| {
		tag = HostValue.new_tag({})
		initial : {} -> HostValue
		initial = |_| HostValue.store_tagged(Box.box(init), tag)
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left_v : a
			left_v = Box.unbox(HostValue.get_tagged(left_hv, tag))
			right_v : a
			right_v = Box.unbox(HostValue.get_tagged(right_hv, tag))
			left_v.is_eq(right_v)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(a)
			boxed = HostValue.take(host_value)
			_ = boxed
			{}
		}
		token = Box.box(0)
		handle : State(a)
		handle = { ref: Node.BinderRef.BinderRef(token), tag }
		child = body(handle)
		Elem.State({ binder: handle.ref, initial: Box.box(initial), eq: Box.box(eq), drop: Box.box(drop), child: Box.box(child) })
	}

	## Introduce a reusable local scope. State/when/each ordinals inside the body
	## are local to this component instance instead of consuming the caller's
	## identity sequence.
	component : ({} -> Elem) -> Elem
	component = |body| Elem.Component({ child: Box.box(body({})) })

	on_change : Signal(a), (a -> Node.Cmd) -> Elem
	on_change = |signal, to_cmd| {
		tag = signal.tag
		wrapped : HostValue -> Node.Cmd
		wrapped = |value| {
			typed : a
			typed = Box.unbox(HostValue.get_tagged(value, tag))
			to_cmd(typed)
		}
		Elem.OnChange({ signal: Signal.to_expr(signal), to_cmd: Box.box(wrapped) })
	}

	on_cleanup : Node.Cleanup -> Elem
	on_cleanup = |cleanup| Elem.Cleanup({ cleanup: cleanup })

	## Conditional. Each arm is its own scope; flipping disposes the losing arm.
	when : Signal(Bool), ({} -> Elem), ({} -> Elem) -> Elem
	when = |condition, when_true, when_false| {
		condition_tag = condition.tag
		read_condition : HostValue -> Bool
		read_condition = |value| Box.unbox(HostValue.get_tagged(value, condition_tag))
		Elem.When(
			{
				condition: Signal.to_expr(condition),
				read: Box.box(read_condition),
				when_true: Box.box(when_true({})),
				when_false: Box.box(when_false({})),
			},
		)
	}

	## Keyed list. `key_of` extracts a typed, stable key per item; `key_hash`
	## produces the host's bucket key; `row` renders a row given that key and a
	## typed signal for the item. Row identity is the key, so per-row local state
	## survives reorder/insert/delete.
	each :
		Signal(List(item)), (item -> k), (k -> U64), (k, Signal(item) -> Elem) -> Elem
			where [
				item.is_eq : item, item -> Bool,
				k.is_eq : k, k -> Bool,
			]
	each = |items, key_of, key_hash, row| {
		items_tag = items.tag
		item_tag = HostValue.new_tag({})
		key_tag = HostValue.new_tag({})
		items_to_values : HostValue -> List(HostValue)
		items_to_values = |items_hv| {
			typed_items : List(item)
			typed_items = Box.unbox(HostValue.get_tagged(items_hv, items_tag))
			List.map(typed_items, |item| HostValue.store_tagged(Box.box(item), item_tag))
		}
		key_of_hv : HostValue -> HostValue
		key_of_hv = |item_hv| {
			item : item
			item = Box.unbox(HostValue.get_tagged(item_hv, item_tag))
			key = key_of(item)
			HostValue.store_tagged(Box.box(key), key_tag)
		}
		key_eq_hv : HostValue, HostValue -> Bool
		key_eq_hv = |left_hv, right_hv| {
			left_k : k
			left_k = Box.unbox(HostValue.get_tagged(left_hv, key_tag))
			right_k : k
			right_k = Box.unbox(HostValue.get_tagged(right_hv, key_tag))
			left_k.is_eq(right_k)
		}
		key_hash_hv : HostValue -> U64
		key_hash_hv = |key_hv| {
			key : k
			key = Box.unbox(HostValue.get_tagged(key_hv, key_tag))
			key_hash(key)
		}
		key_drop_hv : HostValue -> {}
		key_drop_hv = |key_hv| {
			boxed : Box(k)
			boxed = HostValue.take(key_hv)
			_ = boxed
			{}
		}
		item_eq_hv : HostValue, HostValue -> Bool
		item_eq_hv = |left_hv, right_hv| {
			left_item : item
			left_item = Box.unbox(HostValue.get_tagged(left_hv, item_tag))
			right_item : item
			right_item = Box.unbox(HostValue.get_tagged(right_hv, item_tag))
			left_item.is_eq(right_item)
		}
		item_drop_hv : HostValue -> {}
		item_drop_hv = |item_hv| {
			boxed : Box(item)
			boxed = HostValue.take(item_hv)
			_ = boxed
			{}
		}
		row_hv : HostValue, HostValue -> Elem
		row_hv = |key_hv, item_hv| {
			key : k
			key = Box.unbox(HostValue.get_tagged(key_hv, key_tag))
			row_item : {} -> HostValue
			row_item = |_| HostValue.clone(item_hv)
			row_signal_token = Box.box(0)
			row(
				key,
				Signal.from_expr(
					Node.SignalExpr.ConstValue(
						row_signal_token,
						Box.box(row_item),
						Box.box(item_eq_hv),
						Box.box(item_drop_hv),
					),
					item_tag,
				),
			)
		}
		Elem.Each(
			{
				items: Signal.to_expr(items),
				items_to_values: Box.box(items_to_values),
				key_hash: Box.box(key_hash_hv),
				key_of: Box.box(key_of_hv),
				key_eq: Box.box(key_eq_hv),
				key_drop: Box.box(key_drop_hv),
				item_eq: Box.box(item_eq_hv),
				item_drop: Box.box(item_drop_hv),
				row: Box.box(row_hv),
			},
		)
	}
}
