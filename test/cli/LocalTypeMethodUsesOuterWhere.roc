LocalTypeMethodUsesOuterWhere :: [].{}

Apples := [Apples].{
	name : Apples -> Str
	name = |_| "apples"
}

Pears := [Pears].{
	name : Pears -> Str
	name = |_| "pears"
}

# The local type's backing holds the function's type variable, and its
# annotated method calls the method the function's where clause requires of
# that variable. The method's type names the local type, so it generalizes
# with the function and quantifies the function's variable.
describe : a -> Str
	where [a.name : a -> Str]
describe = |x| {
	Local := [L(a)].{
		label : Local -> Str
		label = |L(v)| Str.concat("local ", v.name())
	}
	Local.L(x).label()
}

expect describe(Apples.Apples) == "local apples"

expect describe(Pears.Pears) == "local pears"
