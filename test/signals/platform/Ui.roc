import Elem exposing [Elem]
import HostValue exposing [HostValue]
import Capability exposing [Capability]
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
	State(a) := { ref : Node.BinderRef, cap : Capability(a) }.{
		signal : State(a) -> Signal(a)
		signal = |st| Signal.from_expr(Node.SignalExpr.Ref(st.ref), st.cap)

		## Build a unit-triggered reducer message: `f` maps the current value to the
		## next value, ignoring the unit payload.
			on_unit : State(a), (a -> a) -> Node.Msg
		on_unit = |st, f| {
				current_cap = st.cap
				payload_cap = Capability.new({})
				wrapped : HostValue, HostValue -> HostValue
				wrapped = |current_hv, _payload_hv| {
					current : a
				current = Box.unbox(Capability.get(current_hv, current_cap))
				next : a
					next = f(current)
					Capability.store(Box.box(next), current_cap)
				}
				{ binder: st.ref, payload_kind: Node.unit_payload_kind, payload_accessor: Node.payload_accessor_none, payload_cap: Capability.handle(payload_cap), transform: Box.box(wrapped) }
			}

		on_str : State(a), (a, Str -> a) -> Node.Msg
		on_str = |st, f| {
				current_cap = st.cap
				payload_cap = Capability.new({})
				wrapped : HostValue, HostValue -> HostValue
				wrapped = |current_hv, payload_hv| {
					current : a
				current = Box.unbox(Capability.get(current_hv, current_cap))
				payload : Str
				payload = Box.unbox(Capability.get(payload_hv, payload_cap))
				next : a
					next = f(current, payload)
					Capability.store(Box.box(next), current_cap)
				}
				{ binder: st.ref, payload_kind: Node.str_payload_kind, payload_accessor: Node.payload_accessor_target_value, payload_cap: Capability.handle(payload_cap), transform: Box.box(wrapped) }
			}

		on_bool : State(a), (a, Bool -> a) -> Node.Msg
		on_bool = |st, f| {
				current_cap = st.cap
				payload_cap = Capability.new({})
				wrapped : HostValue, HostValue -> HostValue
				wrapped = |current_hv, payload_hv| {
					current : a
				current = Box.unbox(Capability.get(current_hv, current_cap))
				payload : Bool
				payload = Box.unbox(Capability.get(payload_hv, payload_cap))
				next : a
					next = f(current, payload)
					Capability.store(Box.box(next), current_cap)
				}
				{ binder: st.ref, payload_kind: Node.bool_payload_kind, payload_accessor: Node.payload_accessor_target_checked, payload_cap: Capability.handle(payload_cap), transform: Box.box(wrapped) }
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
			cap = Capability.new({})
				initial : {} -> HostValue
				initial = |_| Capability.store(Box.box(init), cap)
				token : Box(U64)
				token = Node.new_token({})
			handle : State(a)
			handle = { ref: Node.BinderRef.BinderRef(token), cap }
		child = body(handle)
		Elem.State(
			{
				binder: handle.ref,
				initial: Box.box(initial),
				cap: Capability.handle(cap),
				child: Box.box(child),
			},
		)
	}

	## Introduce a reusable local scope. State/when/each ordinals inside the body
	## are local to this component instance instead of consuming the caller's
	## identity sequence.
	component : ({} -> Elem) -> Elem
	component = |body| Elem.Component({ child: Box.box(body({})) })

	on_change : Signal(a), (a -> Node.Cmd) -> Elem
	on_change = |signal, to_cmd| {
		cap = signal.cap
		wrapped : HostValue -> Node.Cmd
		wrapped = |value| {
			typed : a
			typed = Box.unbox(Capability.get(value, cap))
			to_cmd(typed)
		}
		Elem.OnChange({ signal: Signal.to_expr(signal), to_cmd: Box.box(wrapped) })
	}

	on_mount : ({} -> Node.Cmd) -> Elem
	on_mount = |to_cmd| Elem.OnMount({ to_cmd: Box.box(to_cmd) })

	on_cleanup : Node.Cleanup -> Elem
	on_cleanup = |cleanup| Elem.Cleanup({ cleanup: cleanup })

	## Conditional. Each arm is its own scope; flipping disposes the losing arm.
	when : Signal(Bool), ({} -> Elem), ({} -> Elem) -> Elem
	when = |condition, when_true, when_false| {
		condition_cap = condition.cap
		read_condition : HostValue -> Bool
		read_condition = |value| Box.unbox(Capability.get(value, condition_cap))
		Elem.When(
			{
				condition: Signal.to_expr(condition),
				read_cap: Capability.handle(condition_cap),
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
		items_cap = items.cap
		item_cap = Capability.new({})
		key_cap = Capability.new({})
		items_to_values : HostValue -> List(HostValue)
		items_to_values = |items_hv| {
			typed_items : List(item)
			typed_items = Box.unbox(Capability.get(items_hv, items_cap))
			List.map(typed_items, |item| Capability.store(Box.box(item), item_cap))
		}
		key_of_hv : HostValue -> HostValue
		key_of_hv = |item_hv| {
			item : item
			item = Box.unbox(Capability.get(item_hv, item_cap))
			key = key_of(item)
			Capability.store(Box.box(key), key_cap)
		}
		key_hash_hv : HostValue -> U64
		key_hash_hv = |key_hv| {
			key : k
			key = Box.unbox(Capability.get(key_hv, key_cap))
			key_hash(key)
		}
		row_hv : HostValue, HostValue -> Elem
		row_hv = |key_hv, item_hv| {
				key : k
				key = Box.unbox(Capability.get(key_hv, key_cap))
				row_item : {} -> HostValue
				row_item = |_| HostValue.clone(item_hv)
				row_signal_token : Box(U64)
				row_signal_token = Node.new_token({})
				row(
				key,
				Signal.from_expr(
					Node.SignalExpr.ConstValue(
						row_signal_token,
						Box.box(row_item),
						Capability.handle(item_cap),
					),
					item_cap,
				),
			)
		}
		Elem.Each(
			{
				items: Signal.to_expr(items),
				items_to_values: Box.box(items_to_values),
				item_cap: Capability.handle(item_cap),
				key_cap: Capability.handle(key_cap),
				key_hash: Box.box(key_hash_hv),
				key_of: Box.box(key_of_hv),
				row: Box.box(row_hv),
			},
		)
	}
}
