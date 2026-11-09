# Completed constants are data, including nested backings and captures.
# The CLI regression checks that the repeated reads allocate nothing.
table : List({ bytes: List(U8), kind: U64 })
table = [{ bytes: [1, 2, 3], kind: 1 }, { bytes: [4, 5, 6], kind: 2 }, { bytes: [7, 8, 9], kind: 3 }]
flat : List(U64)
flat = [1, 2, 3]
repeated : U64 -> U64
repeated = |n| {
	var $i = 0.U64
	var $sum = 0.U64
	while $i < n {
		entry = match List.get(table, $i % 3) { Ok(x) => x Err(_) => crash "invalid" }
		$sum = $sum + entry.kind
		$i = $i + 1
	}
	$sum
}
bound : U64 -> U64
bound = |n| {
	values = table
	var $i = 0.U64
	var $sum = 0.U64
	while $i < n {
		entry = match List.get(values, $i % 3) { Ok(x) => x Err(_) => crash "invalid" }
		$sum = $sum + entry.kind
		$i = $i + 1
	}
	$sum
}
flat_repeated : U64 -> U64
flat_repeated = |n| {
	var $i = 0.U64
	var $sum = 0.U64
	while $i < n {
		entry = match List.get(flat, $i % 3) { Ok(x) => x Err(_) => crash "invalid" }
		$sum = $sum + entry
		$i = $i + 1
	}
	$sum
}
expect repeated(1023) == 2046
expect repeated(4095) == 8190
expect bound(1023) == 2046
expect bound(4095) == 8190
expect flat_repeated(1023) == 2046
expect flat_repeated(4095) == 8190

boxed = Box.box({ values: table, count: 3.U64 })
make = |captured| |n| captured + n
plus_seven : U64 -> U64
plus_seven = make(7.U64)
functions : List(U64 -> U64)
functions = [plus_seven, make(11.U64)]

expect Box.unbox(boxed).count == 3
expect Box.unbox(boxed).values == table
expect plus_seven(5) == 12
expect match List.get(functions, 1) { Ok(f) => f(2) == 13 Err(_) => Bool.False }
expect {
	changed = table.append({ bytes: [10, 11], kind: 4 })
	changed.len() == 4 and table.len() == 3
}
expect {
	match flat.set(0, 99) {
		Ok(changed) => changed == [99, 2, 3] and flat == [1, 2, 3]
		Err(_) => Bool.False
	}
}

Tree := [Leaf(U64), Branch(Tree, Tree)]
leaf : Tree
leaf = Leaf(7)
tree : Tree
tree = Branch(leaf, leaf)
sum_tree : Tree -> U64
sum_tree = |value| match value {
	Leaf(n) => n
	Branch(left, right) => sum_tree(left) + sum_tree(right)
}
expect sum_tree(tree) == 14
expect sum_tree(leaf) == 7
