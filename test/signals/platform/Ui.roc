import Elem exposing [Elem]
import NodeValue exposing [NodeValue]
import Node
import Signal exposing [Signal]

## Dynamic structure and local state. State is introduced through an explicit
## closure binder (`Ui.state`): the binder is the construction site, which is the
## only way to give per-instance state a stable identity in pure Roc. The host
## assigns construction-order identity by walking the descriptor tree; binders are
## referenced de-Bruijn style (distance out to the enclosing `Ui.state`), so the
## same helper composes correctly wherever it is mounted.
Ui := [].{
	## A handle to a state binder, given to the `Ui.state` body. `signal` reads the
	## current value; `send` builds a `Node.Msg` that, when its event fires, applies
	## the given reducer to the current value.
	State(a) := { ref : Node.BinderRef }.{
		signal : State(a) -> Signal(a)
		signal = |st| Signal.from_expr(Node.SignalExpr.Ref(st.ref))

		## Build a unit-triggered reducer message: `f` maps the current value to the
		## next value, ignoring the (unit) payload.
		on_unit : State(a), (a -> a) -> Node.Msg
			where [
				a.encode : a, NodeValue -> Try(NodeValue, []),
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
			]
		on_unit = |st, f| {
			wrapped : NodeValue, NodeValue -> NodeValue
			wrapped = |current_nv, _payload| {
				A : a
				current : a
				current =
					match A.decode(current_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Ui.state reducer received a value that does not match the state type"
						}
					}
				next : a
				next = f(current)
				match next.encode(NodeValue.format) {
					Ok(encoded) => encoded
				}
			}
			{ binder: st.ref, payload_kind: Node.unit_payload_kind, transform: Box.box(wrapped) }
		}

		## Build a payload-carrying reducer: `f` maps (current, payload) to next.
		on_value : State(a), U64, (a, p -> a) -> Node.Msg
			where [
				a.encode : a, NodeValue -> Try(NodeValue, []),
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
				p.decode : NodeValue, NodeValue -> (Try(p, [TypeMismatch]), NodeValue),
			]
		on_value = |st, payload_kind, f| {
			wrapped : NodeValue, NodeValue -> NodeValue
			wrapped = |current_nv, payload_nv| {
				A : a
				current : a
				current =
					match A.decode(current_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Ui.state reducer received a value that does not match the state type"
						}
					}
				P : p
				payload : p
				payload =
					match P.decode(payload_nv, NodeValue.format) {
						(Ok(value), _) => value
						(Err(_), _) => {
							crash "Ui.state reducer received a payload that does not match the event type"
						}
					}
				next : a
				next = f(current, payload)
				match next.encode(NodeValue.format) {
					Ok(encoded) => encoded
				}
			}
			{ binder: st.ref, payload_kind, transform: Box.box(wrapped) }
		}

		on_str : State(a), (a, Str -> a) -> Node.Msg
			where [
				a.encode : a, NodeValue -> Try(NodeValue, []),
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
			]
		on_str = |st, f| st.on_value(Node.str_payload_kind, f)

		on_bool : State(a), (a, Bool -> a) -> Node.Msg
			where [
				a.encode : a, NodeValue -> Try(NodeValue, []),
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
			]
		on_bool = |st, f| st.on_value(Node.bool_payload_kind, f)
	}

	## Introduce a state binder. `init` is the initial value; `body` receives a
	## `State(a)` handle and returns the subtree built with that state in scope.
	## The host mints this binder's identity by its construction-order position.
	state :
		a, (State(a) -> Elem) -> Elem
			where [
				a.is_eq : a, a -> Bool,
				a.encode : a, NodeValue -> Try(NodeValue, []),
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
			]
	state = |init, body| {
		initial_nv =
			match init.encode(NodeValue.format) {
				Ok(encoded) => encoded
			}
		eq : NodeValue, NodeValue -> Bool
		eq = |left, right| {
			A : a
			left_v : a
			left_v =
				match A.decode(left, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Ui.state equality received a left value that does not match the state type"
					}
				}
			right_v : a
			right_v =
				match A.decode(right, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Ui.state equality received a right value that does not match the state type"
					}
				}
			left_v.is_eq(right_v)
		}
		token = Box.box(0)
		handle : State(a)
		handle = { ref: Node.BinderRef.BinderRef(token) }
		child = body(handle)
		Elem.State({ binder: handle.ref, initial: initial_nv, eq: Box.box(eq), child: Box.box(child) })
	}

	## Conditional. Each arm is its own scope; flipping disposes the losing arm.
	when : Signal(Bool), ({} -> Elem), ({} -> Elem) -> Elem
	when = |condition, when_true, when_false| {
		Elem.When(
			{
				condition: Signal.to_expr(condition),
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
				item.decode : NodeValue, NodeValue -> (Try(item, [TypeMismatch]), NodeValue),
				k.encode : k, NodeValue -> Try(NodeValue, []),
				k.decode : NodeValue, NodeValue -> (Try(k, [TypeMismatch]), NodeValue),
				k.to_hash : k, Hasher -> Hasher,
				k.is_eq : k, k -> Bool,
			]
	each = |items, key_of, row| {
		decode_item : NodeValue -> item
		decode_item = |nv| {
			Item : item
			match Item.decode(nv, NodeValue.format) {
				(Ok(value), _) => value
				(Err(_), _) => {
					crash "Ui.each received an item value that does not match the row type"
				}
			}
		}
		key_of_nv : NodeValue -> NodeValue
		key_of_nv = |nv| {
			key = key_of(decode_item(nv))
			match key.encode(NodeValue.format) {
				Ok(encoded) => encoded
			}
		}
		key_eq_nv : NodeValue, NodeValue -> Bool
		key_eq_nv = |left, right| {
			K : k
			left_k : k
			left_k =
				match K.decode(left, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Ui.each key equality received a left key that does not match the key type"
					}
				}
			right_k : k
			right_k =
				match K.decode(right, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Ui.each key equality received a right key that does not match the key type"
					}
			}
			left_k.is_eq(right_k)
		}
		row_nv : NodeValue, NodeValue -> Elem
		row_nv = |key_nv, item_nv| {
			K : k
			key : k
			key =
				match K.decode(key_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Ui.each received a key value that does not match the key type"
					}
			}
			row(key, Signal.from_expr(Node.SignalExpr.ConstValue(item_nv)))
		}
		Elem.Each(
			{
				items: Signal.to_expr(items),
				key_of: Box.box(key_of_nv),
				key_eq: Box.box(key_eq_nv),
				row: Box.box(row_nv),
			},
		)
	}
}
