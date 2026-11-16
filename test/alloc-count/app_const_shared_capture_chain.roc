app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/13010
# Each node of these constants holds two closures that both capture the node
# before it, so the node before it is shared by both closures. Every chain
# reads back the value it was evaluated to, at every depth, and reading it
# allocates nothing: the constant is stored data, not rebuilt.
Node := [Node({ left : (U64 -> U64), right : (U64 -> U64) })]

leaf : U64 -> Node
leaf = |v| Node.Node({ left: |_| v, right: |_| v })

pair : Node, Node -> Node
pair = |l, r| Node.Node({
	left: |i| if (i % 2 == 0) go_left(l, i // 2) else go_left(r, i // 2),
	right: |i| if (i % 2 == 0) go_right(l, i // 2) else go_right(r, i // 2),
})

go_left : Node, U64 -> U64
go_left = |Node(n), i| (n.left)(i)

go_right : Node, U64 -> U64
go_right = |Node(n), i| (n.right)(i)

chain : U64 -> Node
chain = |depth| if (depth == 0) leaf(1) else pair(chain(depth - 1), leaf(depth))

chain2 : Node
chain2 = chain(2)

chain4 : Node
chain4 = chain(4)

chain6 : Node
chain6 = chain(6)

chain8 : Node
chain8 = chain(8)

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input)

	before2 = Host.alloc_count!()
	v2 = go_left(chain2, n)
	allocs2 = Host.alloc_count!() - before2

	before4 = Host.alloc_count!()
	v4 = go_left(chain4, n)
	allocs4 = Host.alloc_count!() - before4

	before6 = Host.alloc_count!()
	v6 = go_left(chain6, n)
	allocs6 = Host.alloc_count!() - before6

	before8 = Host.alloc_count!()
	v8 = go_right(chain8, n)
	allocs8 = Host.alloc_count!() - before8

	"values: ${v2.to_str()} ${v4.to_str()} ${v6.to_str()} ${v8.to_str()}, allocations: ${allocs2.to_str()} ${allocs4.to_str()} ${allocs6.to_str()} ${allocs8.to_str()}"
}
