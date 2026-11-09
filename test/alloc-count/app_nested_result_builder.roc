app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/12327
#
# A recursive builder that returns its node/child lists nested inside a
# record next to an immutable cursor must still append into uniquely owned
# lists, growing them geometrically just like the flat-result control. Both
# build the same complete binary tree in postorder.

TreeNode : { byte : U8, left : U64, right : U64 }
Builder : { nodes : List(TreeNode), children : List(U64) }
Cursor : { source : List(U8), pos : U64 }
NestedResult : { cursor : Cursor, builder : Builder, node : U64 }
FlatResult : { cursor : Cursor, nodes : List(TreeNode), children : List(U64), node : U64 }

byte_at : Cursor -> U8
byte_at = |cursor|
	match List.get(cursor.source, cursor.pos % List.len(cursor.source)) {
		Ok(b) => b
		Err(_) => 0
	}

build_nested : Cursor, Builder, U64 -> NestedResult
build_nested = |cursor, builder, depth|
	if depth == 0 {
		node = List.len(builder.nodes)
		{
			cursor: { source: cursor.source, pos: cursor.pos + 1 },
			builder: { nodes: builder.nodes.append({ byte: byte_at(cursor), left: node, right: node }), children: builder.children },
			node,
		}
	} else {
		{ cursor: left_cursor, builder: left_builder, node: left_node } = build_nested(cursor, builder, depth - 1)
		{ cursor: right_cursor, builder: right_builder, node: right_node } = build_nested(left_cursor, left_builder, depth - 1)
		node = List.len(right_builder.nodes)
		{
			cursor: { source: right_cursor.source, pos: right_cursor.pos + 1 },
			builder: {
				nodes: right_builder.nodes.append({ byte: byte_at(right_cursor), left: left_node, right: right_node }),
				children: right_builder.children.append(left_node).append(right_node),
			},
			node,
		}
	}

build_flat : Cursor, List(TreeNode), List(U64), U64 -> FlatResult
build_flat = |cursor, nodes, children, depth|
	if depth == 0 {
		node = List.len(nodes)
		{
			cursor: { source: cursor.source, pos: cursor.pos + 1 },
			nodes: nodes.append({ byte: byte_at(cursor), left: node, right: node }),
			children,
			node,
		}
	} else {
		{ cursor: left_cursor, nodes: left_nodes, children: left_children, node: left_node } = build_flat(cursor, nodes, children, depth - 1)
		{ cursor: right_cursor, nodes: right_nodes, children: right_children, node: right_node } = build_flat(left_cursor, left_nodes, left_children, depth - 1)
		node = List.len(right_nodes)
		{
			cursor: { source: right_cursor.source, pos: right_cursor.pos + 1 },
			nodes: right_nodes.append({ byte: byte_at(right_cursor), left: left_node, right: right_node }),
			children: right_children.append(left_node).append(right_node),
			node,
		}
	}

run! : Str => Str
run! = |input| {
	# "string from host" is 16 bytes, so depth is 10: 2047 nodes, 2046 edges.
	depth = Str.count_utf8_bytes(input).to_u64() - 6
	source = Str.to_utf8(input)

	nested_before = Host.alloc_count!()
	nested = build_nested({ source, pos: 0 }, { nodes: [], children: [] }, depth)
	nested_allocs = Host.alloc_count!() - nested_before

	flat_before = Host.alloc_count!()
	flat = build_flat({ source, pos: 0 }, [], [], depth)
	flat_allocs = Host.alloc_count!() - flat_before

	same_tree = nested.builder.nodes == flat.nodes and nested.builder.children == flat.children and nested.node == flat.node
	nested_amortized = nested_allocs <= 64
	flat_amortized = flat_allocs <= 64

	"nodes: ${List.len(nested.builder.nodes).to_str()} ${List.len(flat.nodes).to_str()}, edges: ${List.len(nested.builder.children).to_str()} ${List.len(flat.children).to_str()}, same tree: ${Str.inspect(same_tree)}, nested amortized: ${Str.inspect(nested_amortized)}, flat amortized: ${Str.inspect(flat_amortized)}, allocations: ${nested_allocs.to_str()} ${flat_allocs.to_str()}"
}
