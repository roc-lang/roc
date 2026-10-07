app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

# A function whose result is an interpolated string literal is generic in that
# result: each caller chooses the type whose `from_interpolation` assembles it.
Wrapped := [Wrapped(Str)].{
	from_interpolation : List(Str) -> Try((List(Str) -> Wrapped), [InvalidInterpolation(Str)])
	from_interpolation = |segments| Str.from_interpolation(segments).map_ok(|assemble| |values| Wrapped.Wrapped(assemble(values)))
}

# This `from_interpolation` is generic in the interpolated item type, so only
# the interpolated parts determine it.
Count := [Count(U64)].{
	from_interpolation : List(Str) -> Try((List(item) -> Count), [InvalidInterpolation(Str)])
	from_interpolation = |_segments| Ok(|values| Count.Count(List.len(values)))
}

describe = |x| "value=${Str.inspect(x)}"

around = |f, x| f("a${x}b${x}c")

count_identity : Count -> Count
count_identity = |c| c

str_identity : Str -> Str
str_identity = |s| s

main! = || {
	described : Str
	described = describe(1.I64)
	Stdout.line!(described)
	wrapped : Wrapped
	wrapped = describe("x")
	Stdout.line!(Str.inspect(wrapped))
	Stdout.line!(Str.inspect(around(count_identity, 3.I64)))
	Stdout.line!(around(str_identity, "s"))
}
