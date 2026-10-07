# Boxy (`--specialize=no`) compiles each generic body once, so these values are
# described by the descriptors their generic producers receive.
#
# - A generic interpolation's parts are its iterator's items, so the items are
#   described by the part's own type.
# - A top-level value built by a generic factory is a stored function whose
#   captures describe the factory's type variables.
# - A top-level callable whose body is a block is a value, so every use shares
#   its one type, including the dictionaries the callable it builds needs.

greet = |name| "hi ${name}"

compose : (b -> c), (a -> b) -> (a -> c)
compose = |f, g| |x| f(g(x))

shown = compose(|n| n.to_str(), |n| n + 1.U64)

pair_with = {
	prefix = Str.repeat("a", 2)
	|x| (prefix, x)
}

greeter = {
	prefix = Str.repeat("he", 2)
	|name| "${prefix} ${name}"
}

main! = |args| {
	(p, s) = pair_with(List.len(args) + 1)
	(_, n) = pair_with(List.len(args) + 2)
	echo!("${greet("x")},${shown(List.len(args) + 41)},${p}${s.to_str()}${n.to_str()},${greeter("y")}")
	Ok({})
}
