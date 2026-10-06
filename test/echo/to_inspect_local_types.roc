# `to_inspect` methods of types declared in function bodies whose backings
# name the function's type variables. Inspection reaches them directly,
# through generic helpers, and inside other values; the function's own
# `where` clause supplies what the methods need of those variables.

show = |x| Str.inspect(x)

describe : a -> Str where [a.to_str : a -> Str]
describe = |x| {
	Wrap := { v : a }.{
		to_inspect : Wrap -> Str
		to_inspect = |w| "Wrap(${w.v.to_str()})"
	}

	w = Wrap.{ v: x }
	"${Str.inspect(w)} ${show({ items: [w], tag: Ok(w) })}"
}

# An unannotated method whose argument is a record the backing matches.
pairs : a, b -> Str where [a.to_str : a -> Str, b.to_str : b -> Str]
pairs = |x, y| {
	P(c) := { v : a, c : c }.{
		to_inspect = |p| "P(${p.v.to_str()}, ${p.c.to_str()})"
	}

	"${show(P.{ v: x, c: y })} ${Str.inspect([P.{ v: x, c: 3.U8 }])} ${show(P.{ v: x, c: {} })}"
}

# The same method, annotated.
annotated : a -> Str where [a.to_str : a -> Str]
annotated = |x| {
	Q(c) := { v : a, c : c }.{
		to_inspect : Q(c) -> Str where [c.to_str : c -> Str]
		to_inspect = |q| "Q(${q.v.to_str()}, ${q.c.to_str()})"
	}

	"${show(Q.{ v: x, c: 4.U16 })} ${show(Q.{ v: x, c: {} })}"
}

main! = |_args| {
	echo!("${describe(5.U64)}\n")
	echo!("${describe("s")}\n")
	echo!("${pairs(1.I8, 2.I16)}\n")
	echo!("${annotated(6.I32)}\n")
	Ok({})
}
