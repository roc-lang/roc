import HostValue exposing [HostValue]
import Capability exposing [Capability]
import Node

TaskStatus(a, err) := [Loading, Done(a), Failed(err)]

Task(a, err) := { source : Node.TaskSource, cap : Capability(TaskStatus(a, err)) }

## Opaque, typed signal. Wraps a boxed pure `Node.SignalExpr` descriptor
## referencing state/source binders. The `a` lives only in Roc's type system.
## Runtime values are opaque host-owned cells; each edge carries the exact typed
## thunks that can read, compare, transform, and release that cell.
Signal(a) := { expr : Box(Node.SignalExpr), cap : Capability(a) }.{
	clone_expr : Box(Node.SignalExpr) -> Box(Node.SignalExpr)
	clone_expr = |expr| expr

	to_expr : Signal(a) -> Box(Node.SignalExpr)
	to_expr = |signal| signal.expr

	from_expr : Node.SignalExpr, Capability(a) -> Signal(a)
	from_expr = |expr, cap| { expr: Box.box(expr), cap }

	from_task : Task(a, err) -> Signal(TaskStatus(a, err))
	from_task = |task| { expr: Box.box(Node.SignalExpr.TaskSource(task.source)), cap: task.cap }

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
	fake_task = |name, to_done, to_failed| Signal.task_source(name, to_done, to_failed, True)

	## Low-level host task source constructor. `reset_on_start` controls whether
	## starting a new request publishes `Loading` or keeps the last cached value
	## while the runtime request is pending.
	task_source :
		Str, (Str -> a), (Str -> err), Bool -> Task(a, err)
			where [
				a.is_eq : a, a -> Bool,
				err.is_eq : err, err -> Bool,
			]
	task_source = |name, to_done, to_failed, reset_on_start| {
		token : Box(U64)
		token = Node.new_token({})
		status_cap =
			Capability.new_with_eq(
				|left, right|
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
					},
		)
		payload_cap = Capability.new({})

		loading : TaskStatus(a, err)
		loading = TaskStatus.Loading

		initial : {} -> HostValue
		initial = |_| Capability.store(Box.box(loading), status_cap)

		done : HostValue -> HostValue
		done = |payload_hv| {
			payload : Str
			payload = Box.unbox(Capability.take(payload_hv, payload_cap))
			status : TaskStatus(a, err)
			status = TaskStatus.Done(to_done(payload))
			Capability.store(Box.box(status), status_cap)
		}

		failed : HostValue -> HostValue
		failed = |payload_hv| {
			payload : Str
			payload = Box.unbox(Capability.take(payload_hv, payload_cap))
			status : TaskStatus(a, err)
			status = TaskStatus.Failed(to_failed(payload))
			Capability.store(Box.box(status), status_cap)
		}

		{
				source: {
					token,
					name,
					cap: Capability.handle(status_cap),
					payload_cap: Capability.handle(payload_cap),
					initial: Box.box(initial),
				done: Box.box(done),
				failed: Box.box(failed),
				reset_on_start,
			},
			cap: status_cap,
		}
	}

	start_str : Task(a, err), Str -> Node.Cmd
	start_str = |task, request| {
		request_cap = Capability.new({})
		request_init : {} -> HostValue
		request_init = |_| Capability.store(Box.box(request), request_cap)
		request_read : HostValue -> Str
		request_read = |value| Box.unbox(Capability.get(value, request_cap))
		Node.Cmd.StartTask(
			{
				task_token: task.source.token,
				task_name: task.source.name,
				request_init: Box.box(request_init),
				request_read: { capability: Capability.handle(request_cap), read: Box.box(request_read) },
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
				token : Box(U64)
				token = Node.new_token({})
				cap = Capability.new({})

			initial : {} -> HostValue
			initial = |_| Capability.store(Box.box(initial_value), cap)

			tick : HostValue -> HostValue
			tick = |current_hv| {
				current : a
				current = Box.unbox(Capability.get(current_hv, cap))
				Capability.store(Box.box(next(current)), cap)
			}

			{
				expr: Box.box(
					Node.SignalExpr.IntervalSource(
						{
							token,
							period_ms,
							cap: Capability.handle(cap),
							initial: Box.box(initial),
							tick: Box.box(tick),
						},
					),
				),
				cap,
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
		token : Box(U64)
		token = Node.new_token({})
		cap = Capability.new({})
		init : {} -> HostValue
		init = |_| Capability.store(Box.box(value), cap)
		{
			expr: Box.box(
				Node.SignalExpr.ConstValue(
					token,
					Box.box(init),
					Capability.handle(cap),
				),
			),
			cap,
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
		token : Box(U64)
		token = Node.new_token({})
		output_cap = Capability.new({})
		wrapped : HostValue -> HostValue
		wrapped = |input_hv| {
			typed_input : a
			typed_input = Box.unbox(Capability.get(input_hv, signal.cap))
			typed_output : b
			typed_output = f(typed_input)
			Capability.store(Box.box(typed_output), output_cap)
		}

		{
			expr: Box.box(
				Node.SignalExpr.Map(
					token,
					signal.expr,
					Box.box(wrapped),
					Capability.handle(output_cap),
				),
			),
			cap: output_cap,
		}
	}

	map2 :
		Signal(a), Signal(b), (a, b -> c) -> Signal(c)
			where [
				c.is_eq : c, c -> Bool,
			]
	map2 = |left, right, f| {
		token : Box(U64)
		token = Node.new_token({})
		output_cap = Capability.new({})
		wrapped : HostValue, HostValue -> HostValue
		wrapped = |left_hv, right_hv| {
			left_v : a
			left_v = Box.unbox(Capability.get(left_hv, left.cap))
			right_v : b
			right_v = Box.unbox(Capability.get(right_hv, right.cap))
			output : c
			output = f(left_v, right_v)
			Capability.store(Box.box(output), output_cap)
		}

		{
			expr: Box.box(
				Node.SignalExpr.Map2(
					token,
					left.expr,
					right.expr,
					Box.box(wrapped),
					Capability.handle(output_cap),
				),
			),
			cap: output_cap,
		}
	}

	## Combine a list of same-typed signals into a signal of the list of values.
	combine :
		List(Signal(a)) -> Signal(List(a))
			where [
				a.is_eq : a, a -> Bool,
			]
	combine = |signals| {
		token : Box(U64)
		token = Node.new_token({})
		input_cap =
			match List.first(signals) {
				Ok(first) => first.cap
				Err(_) => Capability.new({})
			}
		output_cap = Capability.new({})
		exprs = List.map(signals, |s| Box.unbox(Signal.clone_expr(s.expr)))
		transform : List(HostValue) -> HostValue
		transform = |items| {
			values : List(a)
			values = List.map(items, |host_value| Box.unbox(Capability.get(host_value, input_cap)))
			Capability.store(Box.box(values), output_cap)
		}
		{
			expr: Box.box(
				Node.SignalExpr.Combine(
					token,
					exprs,
					Box.box(transform),
					Capability.handle(output_cap),
				),
			),
			cap: output_cap,
		}
	}
}
