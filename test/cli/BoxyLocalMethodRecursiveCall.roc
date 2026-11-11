BoxyLocalMethodRecursiveCall :: [].{}

# A generic function passes a value of a type its body declares to a recursive
# call of itself. The type's method reads the function's type variable `a`, so
# the type's identity includes `a` (design.md "Body-Local Type Context"): the
# recursive call uses `describe` at `Local` wrapping `a`, the next level at
# `Local` wrapping that, and so on. That is polymorphic recursion, rejected
# under every lowering strategy.

Apples := [Apples].{
	label : Apples -> Str
	label = |_| "apples"

	make : () -> Apples
	make = || Apples.Apples
}

describe : a, U64 -> Str
	where [a.label : a -> Str, a.make : () -> a]
describe = |x, n| {
	Local := [L].{
		label : Local -> Str
		label = |_| {
			Outer : a
			v : a
			v = Outer.make()
			Str.concat("local of ", v.label())
		}

		make : () -> Local
		make = || Local.L
	}
	if n == 0 {
		x.label()
	} else {
		Str.concat(Str.concat(x.label(), " / "), describe(Local.L, n - 1))
	}
}

expect describe(Apples.make(), 1) == "apples / local of apples"
