# `insert_at` is tail-recursive, so its `t` parameter is also its loop's join
# parameter. Compile-time evaluation passes it `empty`, whose `nodes` list lives
# in static memory: only the back edge stores a fresh list into `t.nodes`, so
# appending to `t.nodes` on the first iteration must not write in place.

Node : { children : List(I64), has_value : Bool }

Trie : { nodes : List(Node) }

empty : Trie
empty = { nodes: [empty_node] }

empty_node : Node
empty_node = { children: [-1, -1, -1, -1], has_value: False }

insert_at : Trie, List(U8), I64, I64, I64 -> Trie
insert_at = |t, key, ki, klen, node_idx| if ki >= klen {
	node = List.get(t.nodes, I64.to_u64_wrap(node_idx)) ?? crash ("get")
	{ nodes: List.set(t.nodes, I64.to_u64_wrap(node_idx), { children: node.children, has_value: True }) ?? crash ("set") }
} else {
	ch = U8.to_i64(List.get(key, I64.to_u64_wrap(ki)) ?? crash ("key"))
	node = List.get(t.nodes, I64.to_u64_wrap(node_idx)) ?? crash ("get")
	child_idx = List.get(node.children, I64.to_u64_wrap(ch)) ?? crash ("get")
	if child_idx < 0 {
		new_idx = U64.to_i64_wrap(List.len(t.nodes))
		new_children = List.set(node.children, I64.to_u64_wrap(ch), new_idx) ?? crash ("set")
		updated = { children: new_children, has_value: node.has_value }
		t2 = { nodes: List.set(List.append(t.nodes, empty_node), I64.to_u64_wrap(node_idx), updated) ?? crash ("set") }
		insert_at(t2, key, ki + 1, klen, new_idx)
	} else {
		insert_at(t, key, ki + 1, klen, child_idx)
	}
}

keys : Trie -> List(List(U8))
keys = |t| if List.len(t.nodes) == 0 {
	[]
} else {
	collect_keys(t, 0, [])
}

collect_keys : Trie, I64, List(List(U8)) -> List(List(U8))
collect_keys = |t, node_idx, acc| {
	node = List.get(t.nodes, I64.to_u64_wrap(node_idx)) ?? crash ("get")
	acc2 = if node.has_value {
		List.append(acc, [])
	} else {
		acc
	}
	collect_children(t, node, 0, acc2)
}

collect_children : Trie, Node, I64, List(List(U8)) -> List(List(U8))
collect_children = |t, node, i, acc| if i >= 4 {
	acc
} else {
	child_idx = List.get(node.children, I64.to_u64_wrap(i)) ?? crash ("get")
	if child_idx < 0 {
		collect_children(t, node, i + 1, acc)
	} else {
		collect_children(t, node, i + 1, collect_keys(t, child_idx, acc))
	}
}

main! = |_args| {
	t = insert_at(empty, [2], 0, 1, 0)
	echo!(U64.to_str(List.len(keys(t))))
	Ok({})
}
