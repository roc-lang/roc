# repro for https://github.com/roc-lang/roc/issues/13340
# A call from another module to a generic function that is in a cycle of tail
# calls passes every parameter the function reads, whether the parameters
# travel in registers (`is_even`) or on the stack (`wide_even`). The count
# comes from the arguments, so the calls run in the built program.
import Issue13340Parity

main! = |args| {
	count = args.len() * 2 + 10
	parity = if Issue13340Parity.is_even(args, count) "even" else "odd"
	sum = Issue13340Parity.wide_even(args, count, 1, 2, 3, 4, 5, 6, 7, 8)
	echo!("${parity} ${sum.to_str()} ${(args.len() * 0).to_str()}\n")
	Ok({})
}
