app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

# Each `to_inspect` here has a type other than `T -> Str` over distinct
# unconstrained type variables, so inspection renders the default form.
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
	Stdout.line!(Str.inspect({ id: 7.I64, note: note }))
	Stdout.line!(Str.inspect([note]))
	Stdout.line!(render(note))
	Stdout.line!(Str.inspect((fixed, Extra.E(2), 3.I64)))
}
