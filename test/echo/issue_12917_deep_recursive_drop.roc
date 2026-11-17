# Values of recursive types built in a loop, far deeper than one native stack
# frame per level allows, and then dropped. The depth comes from `args` so
# that the values are built at runtime.

V := [Leaf, Node(List(V))]

nested_lists : U64 -> V
nested_lists = |n| {
	var $v = V.Leaf
	var $i = 0
	while $i < n {
		$v = V.Node([$v])
		$i = $i + 1
	}
	$v
}

Tree := [Tip, Fork(Tree, Tree)]

spine : U64 -> Tree
spine = |n| {
	var $t = Tree.Tip
	var $i = 0
	while $i < n {
		$t = if $i % 2 == 0 { Tree.Fork($t, Tree.Tip) } else { Tree.Fork(Tree.Tip, $t) }
		$i = $i + 1
	}
	$t
}

Chain := [End, Link(U64, Chain)]

chain : U64 -> Chain
chain = |n| {
	var $c = Chain.End
	var $i = 0
	while $i < n {
		$c = Chain.Link($i, $c)
		$i = $i + 1
	}
	$c
}

Nested := { name : Str, children : List(Nested) }

nested_records : U64 -> Nested
nested_records = |n| {
	var $v = { name: "a name long enough to live on the heap", children: [] }
	var $i = 0
	while $i < n {
		$v = { name: "a name long enough to live on the heap", children: [$v, { name: "a sibling long enough to live on the heap", children: [] }] }
		$i = $i + 1
	}
	$v
}

main! = |args| {
	n = U64.from_str(args.first() ?? "0") ?? 0
	v = nested_lists(n)
	t = spine(n)
	c = chain(n)
	r = nested_records(n)
	lists =
		match v {
			Leaf => "leaf"
			Node(_) => "node"
		}
	trees =
		match t {
			Tip => "tip"
			Fork(_, _) => "fork"
		}
	chains =
		match c {
			End => "end"
			Link(_, _) => "link"
		}
	echo!("${lists} ${trees} ${chains} ${r.children.len().to_str()}\n")
	Ok({})
}
