# Lambda Solved shares one function's type nodes across all of its call sites,
# so a higher-order function applied at the same value types to different
# closures carries the union of their lambda sets. Every call must still
# dispatch to the closure it was given, with that closure's own captures:
# local closures with different captures and bodies, closures built in another
# module, a recursive closure, and closures reaching the call through a record
# and a list.
import LambdaSetDispatchClosures

apply : (I64 -> I64), I64 -> I64
apply = |f, x| f(x)

twice = |f, x| f(f(x))

main! = |_args| {
	offset = 10
	scale = 3
	add_offset = |n| n + offset
	times_scale = |n| n * scale
	echo!("${apply(add_offset, 1).to_str()} ${apply(times_scale, 1).to_str()}\n")
	echo!("${twice(add_offset, 1).to_str()} ${twice(times_scale, 1).to_str()}\n")

	add_five = LambdaSetDispatchClosures.adder(5)
	times_seven = LambdaSetDispatchClosures.multiplier(7)
	echo!("${apply(add_five, 1).to_str()} ${apply(times_seven, 2).to_str()} ${apply(add_offset, 2).to_str()}\n")

	floor = 1
	steps_down = |n| if n <= floor 0 else 1 + steps_down(n - 1)
	echo!("${apply(steps_down, 5).to_str()} ${apply(times_scale, 5).to_str()}\n")

	chosen = { op: times_seven, label: "seven" }
	echo!("${chosen.label} ${apply(chosen.op, 3).to_str()}\n")
	ops = [add_offset, times_scale, add_five]
	echo!("${Str.inspect(ops.map(|op| apply(op, 4)))}\n")
	Ok({})
}
