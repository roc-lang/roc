# Recursive nominal types inspected, compared, and hashed: recursion through a
# tag payload, a record, a tuple, and a list; mutual recursion; a generic
# recursive type; a recursive type through a source-language `Box`, which
# inspection shows; and a recursive type with a custom `to_inspect`. Values are
# inspected directly, nested in other values, and through generic helpers;
# comparisons and hashes recurse through the same recursion points.

Value := [UInt(U64), Pair({ a : Value, b : Value })].{
	is_eq : Value, Value -> Bool
	is_eq = |x, y| match (x, y) {
		(UInt(m), UInt(n)) => m == n
		(Pair(p), Pair(q)) => p.a == q.a and p.b == q.b
		_ => Bool.False
	}
	to_hash : Value, Hasher -> Hasher
	to_hash = |x, h| match x {
		UInt(n) => n.to_hash(h)
		Pair(p) => p.b.to_hash(p.a.to_hash(h))
	}
}

Chain := [End, Link(Chain)].{
	is_eq : Chain, Chain -> Bool
	is_eq = |x, y| match (x, y) {
		(End, End) => Bool.True
		(Link(m), Link(n)) => m == n
		_ => Bool.False
	}
	to_hash : Chain, Hasher -> Hasher
	to_hash = |x, h| match x {
		End => 0.U8.to_hash(h)
		Link(rest) => rest.to_hash(1.U8.to_hash(h))
	}
}

Tup := [Leaf(Str), Fork((Tup, Tup))]

Rose := [Rose(U8, List(Rose))]

Expr := [Num(I64), Neg(Stmt)]
Stmt := [Ret(Expr), Seq(Stmt, Stmt)]

Tree(a) := [Empty, Node(Tree(a), a, Tree(a))]

Boxed := [Bottom(U64), Up(Box(Boxed))]

Shown := [Done, More(Shown)].{
	to_inspect : Shown -> Str
	to_inspect = |s| match s {
		Done => "."
		More(rest) => "+${Str.inspect(rest)}"
	}
}

show = |x| Str.inspect(x)

same = |x, y| if x == y "eq" else "ne"

distinct = |x, y| Set.empty().insert(x).insert(y).len()

main! = |_args| {
	value : Value
	value = Pair({ a: UInt(1), b: Pair({ a: UInt(2), b: UInt(3) }) })
	chain : Chain
	chain = Link(Link(End))
	tup : Tup
	tup = Fork((Leaf("x"), Fork((Leaf("y"), Leaf("z")))))
	rose : Rose
	rose = Rose(1, [Rose(2, []), Rose(3, [Rose(4, [])])])
	expr : Expr
	expr = Neg(Seq(Ret(Num(5)), Ret(Neg(Ret(Num(6))))))
	tree : Tree(Str)
	tree = Node(Node(Empty, "a", Empty), "b", Empty)
	shown : Shown
	shown = More(More(Done))
	boxed : Boxed
	boxed = Up(Box.box(Up(Box.box(Bottom(1)))))

	echo!("${Str.inspect(value)} ${show(value)} ${show({ v: value, l: [value] })}\n")
	echo!("${Str.inspect(chain)} ${show(chain)} ${show([chain, End])}\n")
	echo!("${Str.inspect(tup)} ${show((tup, 1.U8))}\n")
	echo!("${Str.inspect(rose)} ${show(rose)}\n")
	echo!("${Str.inspect(expr)} ${show(expr)}\n")
	echo!("${Str.inspect(tree)} ${show(tree)}\n")
	echo!("${Str.inspect(shown)} ${show(shown)} ${show({ s: shown })}\n")
	echo!("${Str.inspect(boxed)} ${show(boxed)} ${show({ b: Box.box(5.U8) })}\n")
	echo!("${same(value, value)} ${same(value, UInt(1))} ${same(chain, Link(Link(End)))} ${same(chain, End)} ${same([chain], [chain])} ${same((value, 1.U8), (value, 2.U8))}\n")
	echo!("${Str.inspect(distinct(value, value))} ${Str.inspect(distinct(value, UInt(1)))} ${Str.inspect(distinct(chain, Link(Link(End))))} ${Str.inspect(distinct([chain], [End]))}\n")
	Ok({})
}
