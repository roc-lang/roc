LocalTypeEscapeAccepted :: [].{}

Apples := [Apples].{
	name : Apples -> Str
	name = |_| "apples"
}

Pears := [Pears].{
	name : Pears -> Str
	name = |_| "pears"
}

# The local type depends on the outer function's variable. Nested functions
# take and return it, but it never leaves the outer function's body.
describe_twice : a -> Str
	where [a.name : a -> Str]
describe_twice = |x| {
	Local := [L(a)].{
		label : Local -> Str
		label = |L(v)| Str.concat("local ", v.name())
	}
	wrap = |v| Local.L(v)
	wrap_annotated : a -> Local
	wrap_annotated = |v| Local.L(v)
	relabel : Local -> Str
	relabel = |l| l.label()
	both = [wrap(x), wrap_annotated(x)]
	Str.join_with(List.map(both, relabel), ", ")
}

# A local type that depends on no type variable means the same type at every
# instantiation, so it may leave the body.
make_plain : a -> _
make_plain = |_x| {
	Plain := [P].{
		label : Plain -> Str
		label = |_| "plain"
	}
	Plain.P
}

expect describe_twice(Apples.Apples) == "local apples, local apples"

expect describe_twice(Pears.Pears) == "local pears, local pears"

expect {
	both = [make_plain(1.U8), make_plain("x")]
	List.map(both, |p| p.label()) == ["plain", "plain"]
}
