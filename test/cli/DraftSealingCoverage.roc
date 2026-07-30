DraftSealingCoverage := {}

apply_twice : (U64 -> U64), U64 -> U64
apply_twice = |f, value| f(f(value))

with_nested_signature : U64 -> U64
with_nested_signature = |seed| {
	inc : U64 -> U64
	inc = |value| value + 1

	apply_twice(inc, seed)
}

with_expected_closure : U64 -> U64
with_expected_closure = |seed| {
	make_adder : U64 -> (U64 -> U64)
	make_adder = |delta| |value| value + seed + delta

	add_three : U64 -> U64
	add_three = make_adder(3)

	apply_twice(add_three, 1)
}

with_body_local_recursive_type : U64 -> U64
with_body_local_recursive_type = |seed| {
	LocalTree := [Leaf(U64), Branch(Box(LocalTree), Box(LocalTree))]

	sum : LocalTree -> U64
	sum = |tree|
		match tree {
			Leaf(value) => value
			Branch(left, right) => sum(Box.unbox(left)) + sum(Box.unbox(right))
		}

	tree =
		Branch(
			Box.box(Leaf(seed)),
			Box.box(Branch(Box.box(Leaf(2)), Box.box(Leaf(3)))),
		)

	sum(tree)
}

loop_sum : List(U64) -> U64
loop_sum = |items| {
	var $total = 0

	for item in items {
		$total = $total + item
	}

	$total
}

Payload : [Text(Str), Numbers(List(U64))]

structural_eq_value : U64 -> Bool
structural_eq_value = |n| {
	left = { name: "same", payload: Numbers([n, 2, 3]) }
	right = { name: "same", payload: Numbers([n, 2, 3]) }

	left == right
}

structural_hash_lookup : U64 -> Try(Str, [KeyNotFound])
structural_hash_lookup = |n| {
	key = { id: n, label: "item" }
	lookup = { id: n, label: "item" }

	Dict.empty().insert(key, "found").get(lookup)
}

expect {
	with_nested_signature(40) == 42
}

expect {
	with_expected_closure(4) == 15
}

expect {
	with_body_local_recursive_type(7) == 12
}

expect {
	loop_sum([1, 2, 3, 4]) == 10
}

expect {
	structural_eq_value(9)
}

expect {
	structural_hash_lookup(11) == Ok("found")
}
