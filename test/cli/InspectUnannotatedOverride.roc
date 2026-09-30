app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

# Each unannotated `to_inspect` here has a string result, so its type's
# instance whose result is `Str` is `T -> Str` over distinct unconstrained
# type variables, and inspection uses it.
K := [A, B].{
	to_inspect = |K.(k)| "K.(${Str.inspect(k)})"
}

Hidden := [Secret(Str)].{
	to_inspect = |Hidden.(_)| "<hidden>"
}

CreditCard := Str.{
	create = |nb| CreditCard.(nb)
	to_inspect = |CreditCard.(nb)| {
		last_four_digits = nb.to_utf8().take_last(4) |> Str.from_utf8 ?? "****"
		"**** **** **** ${last_four_digits}"
	}
}

Wrap(a) := [W(a)].{
	to_inspect = |Wrap.(W(v))| "Wrap<${Str.inspect(v)}>"
}

# These results cannot be `Str` for every instantiation of the owner: a
# numeral is never `Str`, and interpolating the payload requires it to be
# `Str`. Inspection renders their default form.
Five := [N(I64)].{
	to_inspect = |_| 5
}

Echo(a) := [E(a)].{
	to_inspect = |Echo.(E(v))| "${v}"
}

main! = || {
	Stdout.line!(Str.inspect(K.(A)))
	Stdout.line!(Str.inspect(Hidden.Secret("pw")))
	card = CreditCard.create("1111 2222 3333 1234")
	Stdout.line!(Str.inspect({ user: "sam", card: card }))
	Stdout.line!(Str.inspect([Wrap.W(1.I64)]))
	Stdout.line!(Str.inspect(Five.N(3)))
	Stdout.line!(Str.inspect(Echo.E("x")))
}
