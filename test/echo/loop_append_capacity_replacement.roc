# Replacing the carried list invalidates the capacity measured at loop entry.
f = |limit| {
	var $collector = []
	var $list = List.with_capacity(100)
	var $i = 0.U64
	while $i < limit {
		$list = List.append($list, 42.U8)
		$collector = List.append($collector, $list)
		$list = List.with_capacity(0)
		$i = $i + 1.U64
	}
	$collector
}

main! = |args| {
	arg = args.first() ?? "hello"
	limit = Str.count_utf8_bytes(arg)
	result = f(limit)
	echo!(Str.inspect(result))
	Ok({})
}
