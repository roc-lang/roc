# A tier-2 generalized local value (annotated tag union, implicitly opened by
# polarity) widened at its use inside a where-clause method used to panic
# monotype postcheck: `closeUnquantifiedTagRowExts` closed the binder's
# quantified row extension, so the serialized use/binder relation pair
# disagreed and the specialization-relation replay unified a widened row
# into a closed one.
#   postcheck invariant violated: instantiation widened a closed tag union
Thing := [Mk].{
	get : Thing, encoding -> Try(U8, [Bad, Worse(Str)])
		where [
			encoding.tick : encoding -> U8,
		]
	get = |_t, encoding| {
		x : Try(U8, [Bad])
		x = if encoding.tick() > 100 Err(Bad) else Ok(1)
		Ok(x?)
	}
}

Enc := [E].{
	tick : Enc -> U8
	tick = |_e| 7
}

run! : List(Str) => Try(U8, [Bad, Worse(Str)])
run! = |args| if List.len(args) > 90 Err(Worse("boom")) else Thing.(Mk).get(Enc.(E))

main! = |args| {
	match run!(args) {
		Ok(_) => Ok({})
		Err(_) => Err(Exit(1))
	}
}
