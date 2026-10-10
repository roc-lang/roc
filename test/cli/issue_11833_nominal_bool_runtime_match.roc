# A `Bool` loop flag compared with `== False` is a nominal wrapping a runtime
# local; specialization must treat its constructor as unknown.
f : U64 -> U64
f = |n| if n == 0 0 else {
	var $i = 0
	var $done = False
	while $done == False {
		if $i == n {
			$done = True
		} else {
			$i = $i + 1
		}
	}
	if $done 1 else 0
}

main! = |args| {
	echo!("result: ${Str.inspect(f(List.len(args)))}")
	Ok({})
}
