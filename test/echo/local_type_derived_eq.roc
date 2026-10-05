# Derived `is_eq : _` and `to_hash : _` of generic nominals instantiated with
# a function-body type whose `is_eq` and `to_hash` capture a local, and a
# derived `is_eq : _` on types declared in the function body itself.

Pair(a) := { left : a, right : a }.{
	is_eq : _
	to_hash : _
}

Tagged(a) := [One(a), Two(a, a)].{
	is_eq : _
}

count_distinct = |items| Set.from_list(items).len()

has_item = |items, item| items.contains(item)

main! = |args| {
	slack = List.len(args) + 1
	Approx := { n : U64 }.{
		is_eq = |a, b| a.n / (slack * 4) == b.n / (slack * 4)
		to_hash = |m, hasher| hasher.write_u64(m.n / (slack * 4))
	}

	Box2(x) := { item : x }.{
		is_eq : _
	}

	Plain := { v : U64, tag : Str }.{
		is_eq : _
	}

	a = Approx.{ n: 1 }
	b = Approx.{ n: 2 }
	far = Approx.{ n: 9 }
	pairs = [Pair.{ left: a, right: Approx.{ n: 5 } }, Pair.{ left: b, right: Approx.{ n: 6 } }]
	distinct = count_distinct(pairs)
	found = has_item(pairs, Pair.{ left: Approx.{ n: 3 }, right: Approx.{ n: 7 } })
	boxed = Box2.{ item: a } == Box2.{ item: b }
	boxed_pair = Box2.{ item: Pair.{ left: a, right: a } } == Box2.{ item: Pair.{ left: far, right: a } }
	tagged = Tagged.One(a) == Tagged.One(b)
	tagged_shape = Tagged.Two(a, b) == Tagged.One(b)
	plain = Plain.{ v: 1, tag: "x" } == Plain.{ v: 1, tag: "y" }
	in_closure = (|| Pair.{ left: a, right: b } == Pair.{ left: far, right: b })()
	echo!("${Str.inspect(distinct)} ${Str.inspect(found)} ${Str.inspect(boxed)} ${Str.inspect(boxed_pair)} ${Str.inspect(tagged)} ${Str.inspect(tagged_shape)} ${Str.inspect(plain)} ${Str.inspect(in_closure)}\n")
	Ok({})
}
