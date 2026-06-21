import HostValue exposing [HostValue]
import Node

TaskStatus(a, err) := [Loading, Done(a), Failed(err)]

Task(a, err) := { source : Node.TaskSource, tag : Box(HostValue.TypeTag(TaskStatus(a, err))) }

## Opaque, typed signal. Wraps a boxed pure `Node.SignalExpr` descriptor
## referencing state/source binders. The `a` lives only in Roc's type system.
## Runtime values are opaque host-owned cells; each edge carries the exact typed
## thunks that can read, compare, transform, and release that cell.
Signal(a) := { expr : Box(Node.SignalExpr), tag : Box(HostValue.TypeTag(a)) }.{
	clone_expr : Box(Node.SignalExpr) -> Box(Node.SignalExpr)
	clone_expr = |expr| expr

	to_expr : Signal(a) -> Box(Node.SignalExpr)
	to_expr = |signal| signal.expr

	from_expr : Node.SignalExpr, Box(HostValue.TypeTag(a)) -> Signal(a)
	from_expr = |expr, tag| { expr: Box.box(expr), tag }

	from_task : Task(a, err) -> Signal(TaskStatus(a, err))
	from_task = |task| { expr: Box.box(Node.SignalExpr.TaskSource(task.source)), tag: task.tag }

	fold_task :
		Task(a, err), b, (a -> b), (err -> b) -> Signal(b)
			where [
				b.is_eq : b, b -> Bool,
			]
	fold_task = |task, loading, done, failed| {
		status = Signal.from_task(task)
		Signal.map(
			status,
			|value| match value {
				TaskStatus.Loading => loading
				TaskStatus.Done(done_value) => done(done_value)
				TaskStatus.Failed(err_value) => failed(err_value)
			},
		)
	}

	fake_task :
		Str, (Str -> a), (Str -> err) -> Task(a, err)
			where [
				a.is_eq : a, a -> Bool,
				err.is_eq : err, err -> Bool,
			]
	fake_task = |name, to_done, to_failed| {
		token = Box.box(0)
		status_tag = HostValue.new_tag({})
		payload_tag = HostValue.new_str_payload_tag({})

		loading : TaskStatus(a, err)
		loading = TaskStatus.Loading

		initial : {} -> HostValue
		initial = |_| HostValue.store_tagged(Box.box(loading), status_tag)

		done : HostValue -> HostValue
		done = |payload_hv| {
			payload : Str
			payload = Box.unbox(HostValue.get_tagged(payload_hv, payload_tag))
			status : TaskStatus(a, err)
			status = TaskStatus.Done(to_done(payload))
			HostValue.store_tagged(Box.box(status), status_tag)
		}

		failed : HostValue -> HostValue
		failed = |payload_hv| {
			payload : Str
			payload = Box.unbox(HostValue.get_tagged(payload_hv, payload_tag))
			status : TaskStatus(a, err)
			status = TaskStatus.Failed(to_failed(payload))
			HostValue.store_tagged(Box.box(status), status_tag)
		}

		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left : TaskStatus(a, err)
			left = Box.unbox(HostValue.get_tagged(left_hv, status_tag))
			right : TaskStatus(a, err)
			right = Box.unbox(HostValue.get_tagged(right_hv, status_tag))
			match left {
				TaskStatus.Loading => match right {
					TaskStatus.Loading => True
					_ => False
				}
				TaskStatus.Done(left_value) => match right {
					TaskStatus.Done(right_value) => left_value.is_eq(right_value)
					_ => False
				}
				TaskStatus.Failed(left_error) => match right {
					TaskStatus.Failed(right_error) => left_error.is_eq(right_error)
					_ => False
				}
			}
		}

		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(TaskStatus(a, err))
			boxed = HostValue.take_tagged(host_value, status_tag)
			_ = boxed
			{}
		}

		payload_drop : HostValue -> {}
		payload_drop = |host_value| {
			boxed : Box(Str)
			boxed = HostValue.take_tagged(host_value, payload_tag)
			_ = boxed
			{}
		}

		{
			source: {
				token,
				name,
				payload_tag,
				payload_drop: Box.box(payload_drop),
				initial: Box.box(initial),
				done: Box.box(done),
				failed: Box.box(failed),
				eq: Box.box(eq),
				drop: Box.box(drop),
			},
			tag: status_tag,
		}
	}

	start_str : Task(a, err), Str -> Node.Cmd
	start_str = |task, request| {
		request_tag = HostValue.new_str_payload_tag({})
		request_init : {} -> HostValue
		request_init = |_| HostValue.store_tagged(Box.box(request), request_tag)
		request_read : HostValue -> Str
		request_read = |value| Box.unbox(HostValue.get_tagged(value, request_tag))
		request_drop : HostValue -> {}
		request_drop = |value| {
			boxed : Box(Str)
			boxed = HostValue.take_tagged(value, request_tag)
			_ = boxed
			{}
		}
		Node.Cmd.StartTask(
			{
				task_token: task.source.token,
				task_name: task.source.name,
				request_init: Box.box(request_init),
				request_read: Box.box(request_read),
				request_drop: Box.box(request_drop),
			},
		)
	}

	cleanup : Str -> Node.Cleanup
	cleanup = |name| Node.Cleanup.Cleanup(name)

	interval : U64 -> Signal(U64)
	interval = |period_ms| {
		source_from_tick :
			a, (a -> a) -> Signal(a)
				where [
					a.is_eq : a, a -> Bool,
				]
		source_from_tick = |initial_value, next| {
			token = Box.box(0)
			tag = HostValue.new_tag({})

			initial : {} -> HostValue
			initial = |_| HostValue.store_tagged(Box.box(initial_value), tag)

			tick : HostValue -> HostValue
			tick = |current_hv| {
				current : a
				current = Box.unbox(HostValue.get_tagged(current_hv, tag))
				HostValue.store_tagged(Box.box(next(current)), tag)
			}

			eq : HostValue, HostValue -> Bool
			eq = |left_hv, right_hv| {
				left : a
				left = Box.unbox(HostValue.get_tagged(left_hv, tag))
				right : a
				right = Box.unbox(HostValue.get_tagged(right_hv, tag))
				left.is_eq(right)
			}

			drop : HostValue -> {}
			drop = |host_value| {
				boxed : Box(a)
				boxed = HostValue.take_tagged(host_value, tag)
				_ = boxed
				{}
			}

			{
				expr: Box.box(
					Node.SignalExpr.IntervalSource(
						{
							token,
							period_ms,
							initial: Box.box(initial),
							tick: Box.box(tick),
							eq: Box.box(eq),
							drop: Box.box(drop),
						},
					),
				),
				tag,
			}
		}

		source_from_tick(0, |current| current + 1)
	}

	## A constant signal.
	const : a -> Signal(a)
		where [
			a.is_eq : a, a -> Bool,
		]
	const = |value| {
		token = Box.box(0)
		tag = HostValue.new_tag({})
		init : {} -> HostValue
		init = |_| HostValue.store_tagged(Box.box(value), tag)
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left : a
			left = Box.unbox(HostValue.get_tagged(left_hv, tag))
			right : a
			right = Box.unbox(HostValue.get_tagged(right_hv, tag))
			left.is_eq(right)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(a)
			boxed = HostValue.take_tagged(host_value, tag)
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
			tag,
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
		output_tag = HostValue.new_tag({})
		wrapped : HostValue -> HostValue
		wrapped = |input_hv| {
			typed_input : a
			typed_input = Box.unbox(HostValue.get_tagged(input_hv, signal.tag))
			typed_output : b
			typed_output = f(typed_input)
			HostValue.store_tagged(Box.box(typed_output), output_tag)
		}
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left : b
			left = Box.unbox(HostValue.get_tagged(left_hv, output_tag))
			right : b
			right = Box.unbox(HostValue.get_tagged(right_hv, output_tag))
			left.is_eq(right)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(b)
			boxed = HostValue.take_tagged(host_value, output_tag)
			_ = boxed
			{}
		}

		{
			expr: Box.box(
				Node.SignalExpr.Map(
					token,
					signal.expr,
					Box.box(wrapped),
					Box.box(eq),
					Box.box(drop),
				),
			),
			tag: output_tag,
		}
	}

	map2 :
		Signal(a), Signal(b), (a, b -> c) -> Signal(c)
			where [
				c.is_eq : c, c -> Bool,
			]
	map2 = |left, right, f| {
		token = Box.box(0)
		output_tag = HostValue.new_tag({})
		wrapped : HostValue, HostValue -> HostValue
		wrapped = |left_hv, right_hv| {
			left_v : a
			left_v = Box.unbox(HostValue.get_tagged(left_hv, left.tag))
			right_v : b
			right_v = Box.unbox(HostValue.get_tagged(right_hv, right.tag))
			output : c
			output = f(left_v, right_v)
			HostValue.store_tagged(Box.box(output), output_tag)
		}
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left_v : c
			left_v = Box.unbox(HostValue.get_tagged(left_hv, output_tag))
			right_v : c
			right_v = Box.unbox(HostValue.get_tagged(right_hv, output_tag))
			left_v.is_eq(right_v)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(c)
			boxed = HostValue.take_tagged(host_value, output_tag)
			_ = boxed
			{}
		}

		{
			expr: Box.box(
				Node.SignalExpr.Map2(
					token,
					left.expr,
					right.expr,
					Box.box(wrapped),
					Box.box(eq),
					Box.box(drop),
				),
			),
			tag: output_tag,
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
		input_tag =
			match List.first(signals) {
				Ok(first) => first.tag
				Err(_) => HostValue.new_tag({})
			}
		output_tag = HostValue.new_tag({})
		exprs = List.map(signals, |s| Box.unbox(Signal.clone_expr(s.expr)))
		transform : List(HostValue) -> HostValue
		transform = |items| {
			values : List(a)
			values = List.map(items, |host_value| Box.unbox(HostValue.get_tagged(host_value, input_tag)))
			HostValue.store_tagged(Box.box(values), output_tag)
		}
		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left_items : List(a)
			left_items = Box.unbox(HostValue.get_tagged(left_hv, output_tag))
			right_items : List(a)
			right_items = Box.unbox(HostValue.get_tagged(right_hv, output_tag))
			left_items.is_eq(right_items)
		}
		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(List(a))
			boxed = HostValue.take_tagged(host_value, output_tag)
			_ = boxed
			{}
		}
		{ expr: Box.box(Node.SignalExpr.Combine(token, exprs, Box.box(transform), Box.box(eq), Box.box(drop))), tag: output_tag }
	}
}
