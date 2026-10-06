app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

# A `to_inspect` is an override for a concrete type exactly when it can be
# used at that type `-> Str`. `Nullable(Str)` would need `Str.to_inspect`,
# `Fixed` is usable at `Fixed(I64)` only, and `Extra`'s takes two arguments,
# so inspection renders the default form everywhere else.
Nullable(a) := [Null, NotNull(a)].{
	to_inspect : Nullable(a) -> Str where [a.to_inspect : a -> Str]
	to_inspect = |n| match n {
		Null => "Null"
		NotNull(v) => "NotNull(${v.to_inspect()})"
	}
}

Fixed(a) := [F(a)].{
	to_inspect : Fixed(I64) -> Str
	to_inspect = |_fixed| "fixed"
}

Extra := [E(I64)].{
	to_inspect : Extra, I64 -> Str
	to_inspect = |_extra, _n| "extra"
}

render = |value| Str.inspect({ value: value })

main! = || {
	note : Nullable(Str)
	note = NotNull("x")
	fixed : Fixed(I64)
	fixed = F(1)
	other : Fixed(U8)
	other = F(2)
	Stdout.line!(Str.inspect({ id: 7.I64, note: note }))
	Stdout.line!(Str.inspect([note]))
	Stdout.line!(render(note))
	Stdout.line!(Str.inspect((fixed, Extra.E(2), 3.I64)))
	Stdout.line!(Str.inspect([other]))
}
