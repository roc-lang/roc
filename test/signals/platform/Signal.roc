import NodeValue exposing [NodeValue]
import Node

## Opaque, typed signal. Wraps a boxed pure `Node.SignalExpr` descriptor referencing
## state/source binders. The `a` lives only in Roc's type system; the wire payload
## is the descriptor. Transforms are captured as boxed thunks (confined erasure),
## pinned to the surrounding `Signal(a)`'s value type.
Signal(a) := { expr : Box(Node.SignalExpr) }.{
	clone_expr : Box(Node.SignalExpr) -> Box(Node.SignalExpr)
	clone_expr = |expr| Box.box(Box.unbox(expr))

	to_expr : Signal(a) -> Box(Node.SignalExpr)
	to_expr = |signal| Signal.clone_expr(signal.expr)

	from_expr : Node.SignalExpr -> Signal(a)
	from_expr = |expr| { expr: Box.box(expr) }

	## A constant signal.
	const : a -> Signal(a) where [a.encode : a, NodeValue -> Try(NodeValue, [])]
	const = |value| {
		nv =
			match value.encode(NodeValue.format) {
				Ok(encoded) => encoded
			}
		{ expr: Box.box(Node.SignalExpr.ConstValue(nv)) }
	}

	const_i64 : I64 -> Signal(I64)
	const_i64 = |value| { expr: Box.box(Node.SignalExpr.ConstValue(NvI64(value))) }

	const_str : Str -> Signal(Str)
	const_str = |value| { expr: Box.box(Node.SignalExpr.ConstValue(NvStr(value))) }

	const_bool : Bool -> Signal(Bool)
	const_bool = |value| { expr: Box.box(Node.SignalExpr.ConstValue(NvBool(value))) }

	## Derived signal. The transform is a typed `a -> b`; we wrap it to decode the
	## input payload and encode the output, pinning both to the call site's types.
	map :
		Signal(a), (a -> b) -> Signal(b)
			where [
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
				b.encode : b, NodeValue -> Try(NodeValue, []),
				b.decode : NodeValue, NodeValue -> (Try(b, [TypeMismatch]), NodeValue),
				b.is_eq : b, b -> Bool,
			]
	map = |signal, f| {
		wrapped : NodeValue -> NodeValue
		wrapped = |input_nv| {
			A : a
			typed_input : a
			typed_input =
				match A.decode(input_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Signal.map received a value that does not match the source signal type"
					}
				}
			typed_output : b
			typed_output = f(typed_input)
			match typed_output.encode(NodeValue.format) {
				Ok(encoded) => encoded
			}
		}
		eq : NodeValue, NodeValue -> Bool
		eq = |left_nv, right_nv| {
			B : b
			left : b
			left =
				match B.decode(left_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Signal.map equality received a left value that does not match the output type"
					}
				}
			right : b
			right =
				match B.decode(right_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Signal.map equality received a right value that does not match the output type"
					}
				}
			left.is_eq(right)
		}

		{
			expr: Box.box(Node.SignalExpr.Map(
				Signal.clone_expr(signal.expr),
				Box.box(wrapped),
				Box.box(eq),
			)),
		}
	}

	map2 :
		Signal(a), Signal(b), (a, b -> c) -> Signal(c)
			where [
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
				b.decode : NodeValue, NodeValue -> (Try(b, [TypeMismatch]), NodeValue),
				c.encode : c, NodeValue -> Try(NodeValue, []),
				c.decode : NodeValue, NodeValue -> (Try(c, [TypeMismatch]), NodeValue),
				c.is_eq : c, c -> Bool,
			]
	map2 = |left, right, f| {
		wrapped : NodeValue, NodeValue -> NodeValue
		wrapped = |left_nv, right_nv| {
			A : a
			left_v : a
			left_v =
				match A.decode(left_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Signal.map2 received a value that does not match the left source signal type"
					}
				}
			B : b
			right_v : b
			right_v =
				match B.decode(right_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Signal.map2 received a value that does not match the right source signal type"
					}
				}
			output : c
			output = f(left_v, right_v)
			match output.encode(NodeValue.format) {
				Ok(encoded) => encoded
			}
		}
		eq : NodeValue, NodeValue -> Bool
		eq = |left_nv, right_nv| {
			C : c
			left_v : c
			left_v =
				match C.decode(left_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Signal.map2 equality received a left value that does not match the output type"
					}
				}
			right_v : c
			right_v =
				match C.decode(right_nv, NodeValue.format) {
					(Ok(value), _) => value
					(Err(_), _) => {
						crash "Signal.map2 equality received a right value that does not match the output type"
					}
				}
			left_v.is_eq(right_v)
		}

		{
			expr: Box.box(Node.SignalExpr.Map2(
				Signal.clone_expr(left.expr),
				Signal.clone_expr(right.expr),
				Box.box(wrapped),
				Box.box(eq),
			)),
		}
	}

	## Combine a list of same-typed signals into a signal of the list of values.
	combine :
		List(Signal(a)) -> Signal(List(a))
			where [
				a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
				a.is_eq : a, a -> Bool,
			]
	combine = |signals| {
		exprs = List.map(signals, |s| Box.unbox(Signal.clone_expr(s.expr)))
		eq : NodeValue, NodeValue -> Bool
		eq = |left_nv, right_nv| {
			A : a
			decode_one = |nv, fmt| A.decode(nv, fmt)
			(left_result, _) = NodeValue.decode_list(NodeValue.format, left_nv, decode_one)
			(right_result, _) = NodeValue.decode_list(NodeValue.format, right_nv, decode_one)
			match left_result {
				Ok(left_items) => {
					left_list : List(a)
					left_list = left_items
					match right_result {
						Ok(right_items) => {
							right_list : List(a)
							right_list = right_items
							left_list.is_eq(right_list)
						}
						Err(_) => {
							crash "Signal.combine equality received a right value that does not match the output type"
						}
					}
				}

				Err(_) => {
					crash "Signal.combine equality received a left value that does not match the output type"
				}
			}
		}
		{ expr: Box.box(Node.SignalExpr.Combine(exprs, Box.box(eq))) }
	}
}
