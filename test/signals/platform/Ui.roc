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
	read_bool : HostValue -> Bool
	read_bool = |value| Box.unbox(HostValue.get(value))

	## A handle to a state binder, given to the `Ui.state` body. `signal` reads the
	## current value; `send` builds a `Node.Msg` that, when its event fires, applies
	## the given reducer to the current value.
	State(a) := { ref : Node.BinderRef }.{
		signal : State(a) -> Signal(a)
		signal = |st| Signal.from_expr(Node.SignalExpr.Ref(st.ref))

		## Build a unit-triggered reducer message: `f` maps the current value to the
		## next value, ignoring the unit payload.
		on_unit : State(a), (a -> a) -> Node.Msg
		on_unit = |st, f| {
			wrapped : HostValue, HostValue -> HostValue
			wrapped = |current_hv, _payload_hv| {
				current : a
				current = Box.unbox(HostValue.get(current_hv))
				next : a
				next = f(current)
				HostValue.store(Box.box(next))
			}
			payload_drop : HostValue -> {}
			payload_drop = |payload_hv| {
				boxed : Box({})
				boxed = HostValue.take(payload_hv)
				_ = boxed
				{}
			}
			{ binder: st.ref, payload_kind: Node.unit_payload_kind, payload_drop: Box.box(payload_drop), transform: Box.box(wrapped) }
		}

		## Build a payload-carrying reducer: `f` maps (current, payload) to next.
		on_value : State(a), U64, (a, p -> a) -> Node.Msg
		on_value = |st, payload_kind, f| {
			wrapped : HostValue, HostValue -> HostValue
			wrapped = |current_hv, payload_hv| {
				current : a
				current = Box.unbox(HostValue.get(current_hv))
				payload : p
				payload = Box.unbox(HostValue.get(payload_hv))
				next : a
				next = f(current, payload)
				HostValue.store(Box.box(next))
			}
			payload_drop : HostValue -> {}
			payload_drop = |payload_hv| {
				boxed : Box(p)
				boxed = HostValue.take(payload_hv)
				_ = boxed
				{}
			}
			{ binder: st.ref, payload_kind, payload_drop: Box.box(payload_drop), transform: Box.box(wrapped) }
		}

		on_str : State(a), (a, Str -> a) -> Node.Msg
		on_str = |st, f| st.on_value(Node.str_payload_kind, f)

		on_bool : State(a), (a, Bool -> a) -> Node.Msg
		on_bool = |st, f| st.on_value(Node.bool_payload_kind, f)
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
		initial : {} -> HostValue
		initial = |_| HostValue.store(Box.box(init))
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left_v : a
			left_v = Box.unbox(HostValue.get(left_hv))
			right_v : a
			right_v = Box.unbox(HostValue.get(right_hv))
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
		handle = { ref: Node.BinderRef.BinderRef(token) }
		child = body(handle)
		Elem.State({ binder: handle.ref, initial: Box.box(initial), eq: Box.box(eq), drop: Box.box(drop), child: Box.box(child) })
	}

	## Conditional. Each arm is its own scope; flipping disposes the losing arm.
	when : Signal(Bool), ({} -> Elem), ({} -> Elem) -> Elem
	when = |condition, when_true, when_false| {
		Elem.When(
			{
				condition: Signal.to_expr(condition),
				read: Box.box(Ui.read_bool),
				when_true: Box.box(when_true({})),
				when_false: Box.box(when_false({})),
			},
		)
	}

	## Keyed list. `key_of` extracts a typed, stable key per item; `row` renders a
	## row given that key and a typed signal for the item. Row identity is the key
	## (compared by the key type's `is_eq`), so per-row local state survives
	## reorder/insert/delete.
	each :
		Signal(List(item)), (item -> k), (k, Signal(item) -> Elem) -> Elem
			where [
				item.is_eq : item, item -> Bool,
				k.is_eq : k, k -> Bool,
			]
	each = |items, key_of, row| {
		items_to_values : HostValue -> List(HostValue)
		items_to_values = |items_hv| {
			typed_items : List(item)
			typed_items = Box.unbox(HostValue.get(items_hv))
			List.map(typed_items, |item| HostValue.store(Box.box(item)))
		}
		key_of_hv : HostValue -> HostValue
		key_of_hv = |item_hv| {
			item : item
			item = Box.unbox(HostValue.get(item_hv))
			key = key_of(item)
			HostValue.store(Box.box(key))
		}
		key_eq_hv : HostValue, HostValue -> Bool
		key_eq_hv = |left_hv, right_hv| {
			left_k : k
			left_k = Box.unbox(HostValue.get(left_hv))
			right_k : k
			right_k = Box.unbox(HostValue.get(right_hv))
			left_k.is_eq(right_k)
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
			left_item = Box.unbox(HostValue.get(left_hv))
			right_item : item
			right_item = Box.unbox(HostValue.get(right_hv))
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
			key = Box.unbox(HostValue.get(key_hv))
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
				),
			)
		}
		Elem.Each(
			{
				items: Signal.to_expr(items),
				items_to_values: Box.box(items_to_values),
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
