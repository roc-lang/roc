# Slots no evidence reaches seal to the empty tag union in Monotype. Lambda
# Solved unifies them exactly like any other type: a function value that is
# inspected but never called keeps its phantom argument types, and an empty
# list shares its function with a populated one.
add = |x, y| x + y

count = |items| List.len(items)

total : List(I64) -> I64
total = |items| List.fold(items, 0, |sum, item| sum + item)

main! = |_args| {
	echo!("${Str.inspect(add)}\n")
	local_add = |x, y| x + y
	echo!("${Str.inspect({ op: local_add, unused: [] })}\n")
	echo!("${Str.inspect(count([]))} ${Str.inspect(count([1, 2]))}\n")
	echo!("${Str.inspect(total([]))} ${Str.inspect(total([3, 4]))}\n")
	echo!("${Str.inspect(List.map([], |n| n + 1))}\n")
	Ok({})
}
