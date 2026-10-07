## Open tag rows compared, hashed, and inspected inside generic functions,
## whose extension's tags are known only at runtime.

Weird := [W(I64)].{
	is_eq : Weird, Weird -> Bool
	is_eq = |_a, _b| True
	to_hash : Weird, Hasher -> Hasher
	to_hash = |_w, h| h
	to_inspect : Weird -> Str
	to_inspect = |_w| "weird"
}

Loose := [L(I64)].{
	is_eq = |_a, _b| True
}

same = |a, b| if a == Nope { False } else { a == b }

wrap = |y| Res(Ok(y))

wrap_dict = |k, v| D(Dict.single(k, v))

count_nope = |items| {
	var $n = 0
	for b in items {
		if b == Nope {
			$n = $n + 1
		} else {
			{}
		}
	}
	$n
}

distinct = |items| {
	var $d = Dict.empty()
	for b in items {
		if b == Nope {
			{}
		} else {
			{}
		}
		$d = $d.insert(b, {})
	}
	$d.len()
}

describe = |items| {
	var $out = ""
	for b in items {
		$out = match b {
			Nope => Str.concat($out, "nope;")
			_ => Str.concat($out, Str.concat(Str.inspect(b), ";"))
		}
	}
	$out
}

sum_below = |limit| {
	var $count = 0
	var $sum = 0
	while $count < limit {
		$sum = $sum + $count
		$count = $count + 1
	}
	$sum
}

main! = |_args| {
	results = [
		Str.inspect(same(Yes(1), Yes(1))),
		Str.inspect(same(Yes(1), Yes(2))),
		Str.inspect(same(Pair(Weird.W(1), "x"), Pair(Weird.W(2), "x"))),
		Str.inspect(same(Pair(Weird.W(1), "x"), Pair(Weird.W(2), "y"))),
		Str.inspect(same(Pair(Loose.L(1), True), Pair(Loose.L(2), True))),
		Str.inspect(same(Rec({ a: 1.5, b: [1, 2] }), Rec({ a: 1.5, b: [1, 2] }))),
		Str.inspect(same(wrap(Weird.W(1)), wrap(Weird.W(2)))),
		Str.inspect(same(wrap("a"), wrap("b"))),
		Str.inspect(same(wrap_dict(1, "a"), wrap_dict(1, "a"))),
		Str.inspect(same(wrap_dict(1, "a"), wrap_dict(1, "b"))),
		Str.inspect(count_nope([Nope, Yes(2), Nope])),
		Str.inspect(distinct([Nope, Yes(1), Nope, Yes(2), Yes(1)])),
		Str.inspect(distinct([Nope, P({ a: "x", b: [1.5] }), P({ a: "x", b: [1.5] }), Q(True), Q(False)])),
		Str.inspect(distinct([Nope, Pair(Weird.W(1), "x"), Pair(Weird.W(2), "x"), Pair(Weird.W(3), "y")])),
		describe([Nope, Yes(2), Pair(Weird.W(1), "x")]),
		Str.inspect(sum_below(5)),
	]
	echo!(Str.join_with(results, ","))
	Ok({})
}
