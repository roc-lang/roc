# A compile-time constant whose code calls a callable value is lowered with
# every variant of that value's type, including one the constant never passes
# and so never runs: `early` passes only `doubling`, but `apply` can call
# `reading`, which reads `deep`, a constant evaluated at a later level than
# `early`. Lowering `early` reserves that slot rather than demanding its value.
base = 10.I64
base_again = base + 1
deep = base_again + 1

reading = |x| x + deep
doubling = |x| x * 2
apply = |f, x| f(x)

early = apply(doubling, base)

main! = |_args| {
	echo!(Str.inspect(early + apply(reading, 1)))
	Ok({})
}
