# Function values at each boundary where Lambda Solved commits a callable's
# representation: boxed and unboxed, inside a top-level record constant whose
# layout is requested, and inside a compile-time-materialized list of closures
# (the host ABI boundary is covered by test/provided-callable-host). Each must
# still call the function it holds.
table : { inc : I64 -> I64, dbl : I64 -> I64 }
table = { inc: |n| n + 1, dbl: |n| n * 2 }

steps : List(I64 -> I64)
steps = [|n| n + 100, |n| n * 3]

run_all : List(I64 -> I64), I64 -> I64
run_all = |fns, start| List.fold(fns, start, |acc, f| f(acc))

main! = |_args| {
	bump = 7
	boxed = Box.box(|n| n + bump)
	unboxed = Box.unbox(boxed)
	echo!("${unboxed(1).to_str()}\n")
	echo!("${(table.inc)(1).to_str()} ${(table.dbl)(5).to_str()}\n")
	echo!("${run_all(steps, 1).to_str()}\n")
	boxed_list = Box.box([table.dbl, |n| n - bump])
	echo!("${run_all(Box.unbox(boxed_list), 10).to_str()}\n")
	Ok({})
}
