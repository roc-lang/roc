app [main!] { pf: platform "./platform/main.roc" }

# Unannotated methods of a recursive nominal: each takes a structural parameter
# and recurses through a method call, so the method's scheme requires itself.
Expr := [Leaf(Str), Next(Expr)].{
	is_eq = |left, right|
		match (left, right) {
			(Leaf(a), Leaf(b)) => a == b
			(Next(a), Next(b)) => a == b
			_ => False
		}

	count = |e, n| match e {
		Leaf(_) => n + 1
		Next(inner) => inner.count(n)
	}

	keep = |e, n| match e {
		Leaf(_) => n
		Next(inner) => inner.keep(n)
	}

	even = |e, n| match e {
		Leaf(_) => n + 1
		Next(inner) => inner.odd(n)
	}

	odd = |e, n| match e {
		Leaf(_) => n + 2
		Next(inner) => inner.even(n)
	}

	both = |left, right| match (left, right) {
		(Next(a), Next(b)) => a.depth() + b.depth()
		_ => 100
	}

	depth = |e| match e {
		Leaf(_) => 0.U64
		Next(inner) => 1 + inner.depth()
	}
}

generic_keep = |x| Expr.Next(Expr.Leaf("a")).keep(x)

late_count = |e, x| e.count(x)

main! = || {
	a = Expr.Next(Expr.Next(Expr.Leaf("a")))
	b = Expr.Next(Expr.Next(Expr.Leaf("a")))
	c = Expr.Next(Expr.Next(Expr.Leaf("c")))
	d = Expr.Next(Expr.Leaf("a"))
	two : U64
	two = 2
	counted : U64
	counted = a.count(2)
	kept : Str
	kept = a.keep("kept")
	even : U64
	even = d.even(3)
	"${Str.inspect(a == b)} ${Str.inspect(a == c)} ${counted.to_str()} ${late_count(a, two).to_str()} ${kept} ${generic_keep(two).to_str()} ${generic_keep("generic")} ${even.to_str()} ${a.both(c).to_str()}"
}
