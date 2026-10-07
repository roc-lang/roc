# Hoisted bindings that reach only stable code through a generic helper, a
# method dispatch, and a closure stored in a top-level constant are evaluated
# normally in every lowering mode.
main! = |_args| {
	echo!("${apply(add_one, 2).to_str()} ${Wrap.Wrap(3).bump().to_str()} ${(fns.f)(4).to_str()}\n")
	Ok({})
}

apply : (a -> b), a -> b
apply = |f, x| f(x)

add_one : I64 -> I64
add_one = |n| n + 1

fns = { f: add_one, n: 1 }

Wrap := [Wrap(I64)].{
	bump = |Wrap(n)| add_one(n)
}
