BoxyLocalMethodRecursiveCall :: [].{}

# A generic function passes a value of a type its body declares to a recursive
# call of itself: the call's dictionary is a method of that type, a local
# procedure of this body, built with this body's type variables rather than
# the call's.

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
