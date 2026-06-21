import HostValue exposing [HostValue]
import Node

## Opaque, typed signal. Wraps a boxed pure `Node.SignalExpr` descriptor
## referencing state/source binders. The `a` lives only in Roc's type system.
## Runtime values are opaque host-owned cells; each edge carries the exact typed
## thunks that can read, compare, transform, and release that cell.
Signal(a) := { expr : Box(Node.SignalExpr) }.{
	clone_expr : Box(Node.SignalExpr) -> Box(Node.SignalExpr)
	clone_expr = |expr| Box.box(Box.unbox(expr))

	to_expr : Signal(a) -> Box(Node.SignalExpr)
	to_expr = |signal| Signal.clone_expr(signal.expr)

	from_expr : Node.SignalExpr -> Signal(a)
	from_expr = |expr| { expr: Box.box(expr) }

	## A constant signal.
	const : a -> Signal(a)
		where [
			a.is_eq : a, a -> Bool,
		]
	const = |value| {
		token = Box.box(0)
		init : {} -> HostValue
		init = |_| HostValue.store(Box.box(value))
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left : a
			left = Box.unbox(HostValue.get(left_hv))
			right : a
			right = Box.unbox(HostValue.get(right_hv))
			left.is_eq(right)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(a)
			boxed = HostValue.take(host_value)
			_ = boxed
			{}
		}
		{
			expr: Box.box(
				Node.SignalExpr.ConstValue(
					token,
					Box.box(init),
					Box.box(eq),
					Box.box(drop),
				),
			),
		}
	}

	## Derived signal. The transform is a typed `a -> b`; the host passes opaque
	## cells and this thunk is the only place that can read the `a` input and
	## construct the `b` output cell.
	map :
		Signal(a), (a -> b) -> Signal(b)
			where [
				b.is_eq : b, b -> Bool,
			]
	map = |signal, f| {
		token = Box.box(0)
		wrapped : HostValue -> HostValue
		wrapped = |input_hv| {
			typed_input : a
			typed_input = Box.unbox(HostValue.get(input_hv))
			typed_output : b
			typed_output = f(typed_input)
			HostValue.store(Box.box(typed_output))
		}
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left : b
			left = Box.unbox(HostValue.get(left_hv))
			right : b
			right = Box.unbox(HostValue.get(right_hv))
			left.is_eq(right)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(b)
			boxed = HostValue.take(host_value)
			_ = boxed
			{}
		}

		{
			expr: Box.box(
				Node.SignalExpr.Map(
					token,
					Signal.clone_expr(signal.expr),
					Box.box(wrapped),
					Box.box(eq),
					Box.box(drop),
				),
			),
		}
	}

	map2 :
		Signal(a), Signal(b), (a, b -> c) -> Signal(c)
			where [
				c.is_eq : c, c -> Bool,
			]
	map2 = |left, right, f| {
		token = Box.box(0)
		wrapped : HostValue, HostValue -> HostValue
		wrapped = |left_hv, right_hv| {
			left_v : a
			left_v = Box.unbox(HostValue.get(left_hv))
			right_v : b
			right_v = Box.unbox(HostValue.get(right_hv))
			output : c
			output = f(left_v, right_v)
			HostValue.store(Box.box(output))
		}
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left_v : c
			left_v = Box.unbox(HostValue.get(left_hv))
			right_v : c
			right_v = Box.unbox(HostValue.get(right_hv))
			left_v.is_eq(right_v)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(c)
			boxed = HostValue.take(host_value)
			_ = boxed
			{}
		}

		{
			expr: Box.box(
				Node.SignalExpr.Map2(
					token,
					Signal.clone_expr(left.expr),
					Signal.clone_expr(right.expr),
					Box.box(wrapped),
					Box.box(eq),
					Box.box(drop),
				),
			),
		}
	}

	## Combine a list of same-typed signals into a signal of the list of values.
	combine :
		List(Signal(a)) -> Signal(List(a))
			where [
				a.is_eq : a, a -> Bool,
			]
	combine = |signals| {
		token = Box.box(0)
		exprs = List.map(signals, |s| Box.unbox(Signal.clone_expr(s.expr)))
		transform : List(HostValue) -> HostValue
		transform = |items| {
			values : List(a)
			values = List.map(items, |host_value| Box.unbox(HostValue.get(host_value)))
			HostValue.store(Box.box(values))
		}
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left_items : List(a)
			left_items = Box.unbox(HostValue.get(left_hv))
			right_items : List(a)
			right_items = Box.unbox(HostValue.get(right_hv))
			left_items.is_eq(right_items)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(List(a))
			boxed = HostValue.take(host_value)
			_ = boxed
			{}
		}
		{ expr: Box.box(Node.SignalExpr.Combine(token, exprs, Box.box(transform), Box.box(eq), Box.box(drop))) }
	}
}
