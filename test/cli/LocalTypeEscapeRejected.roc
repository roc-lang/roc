LocalTypeEscapeRejected :: [].{}

# Each function below declares a type in its body that depends on one of the
# function's own type variables, and lets values of that type leave the body
# through the function's type. Every instantiation of the function would give
# the type a different meaning under the same name, so each is rejected.

Apples := [Apples].{
	name : Apples -> Str
	name = |_| "apples"
}

Pears := [Pears].{
	name : Pears -> Str
	name = |_| "pears"
}

# The local type's method reads the function's type variable.
direct : a -> _
direct = |_x| {
	Local := [L].{
		label : Local -> Str
		label = |_| {
			empty : List(a)
			empty = []
			Str.repeat("!", List.len(empty))
		}
	}
	Local.L
}

in_record : a -> _
	where [a.name : a -> Str]
in_record = |x| {
	Local := [L(a)].{
		label : Local -> Str
		label = |L(v)| v.name()
	}
	{ value: Local.L(x) }
}

in_list : a -> _
	where [a.name : a -> Str]
in_list = |x| {
	Local := [L(a)].{
		label : Local -> Str
		label = |L(v)| v.name()
	}
	[Local.L(x)]
}

in_tag : a -> _
	where [a.name : a -> Str]
in_tag = |x| {
	Local := [L(a)].{
		label : Local -> Str
		label = |L(v)| v.name()
	}
	Boxed(Local.L(x))
}

in_closure : a -> _
	where [a.name : a -> Str]
in_closure = |x| {
	Local := [L(a)].{
		label : Local -> Str
		label = |L(v)| v.name()
	}
	|{}| Local.L(x)
}

# The nested function may return the local type inside the outer body, since
# the type depends on the outer function's variable, but the outer function
# returns it too.
outer : a -> _
	where [a.name : a -> Str]
outer = |x| {
	Local := [L(a)].{
		label : Local -> Str
		label = |L(v)| v.name()
	}
	wrap : a -> Local
	wrap = |v| Local.L(v)
	wrap(x)
}

# The type depends on the nested function's own variable, so it may not leave
# the nested function, even into the body around it.
nested_owner : U64 -> Str
nested_owner = |n| {
	make : b -> _
	make = |_y| {
		Local := [L].{
			label : Local -> Str
			label = |_| {
				empty : List(b)
				empty = []
				Str.repeat("?", List.len(empty))
			}
		}
		Local.L
	}
	Str.concat(make(n).label(), make("s").label())
}

# The nested function's body puts values of its local type into a list the
# body around it owns, so the list mixes the types of both instantiations.
mixed_local : U64 -> U64
mixed_local = |n| {
	acc = []
	add : b -> U64
	add = |_y| {
		Local := [L].{
			label : Local -> Str
			label = |_| {
				empty : List(b)
				empty = []
				Str.repeat("?", List.len(empty))
			}
		}
		List.len(List.append(acc, Local.L))
	}
	add(1.U8) + add("s") + n
}

expect direct(Apples.Apples).label() == ""

expect {
	both = [in_record(Apples.Apples).value, in_record(Pears.Pears).value]
	List.map(both, |l| l.label()) == ["apples", "pears"]
}

expect List.len(in_list(Apples.Apples)) == 1

expect {
	boxed = in_tag(Pears.Pears)
	match boxed {
		Boxed(l) => l.label() == "pears"
	}
}

expect in_closure(Apples.Apples)({}).label() == "apples"

expect outer(Pears.Pears).label() == "pears"

expect nested_owner(1) == ""

expect mixed_local(1) == 3
