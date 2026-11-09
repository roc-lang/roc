# A recursive nominal whose `Dict` field is named through an alias, built by a
# compile-time constant of combinators defined in another module.
import Combinator

Tree :: {}.{
	Props : Dict(Str, List(Str))
	T := { props : Props, kids : List(T) }.{
		is_eq : _
	}

	make : List(U8) -> T
	make = |s| tree.run(s)

	empty : Str -> Try(T, [Failed(Str)])
	empty = |_| Ok({ props: Dict.empty(), kids: [] })
}

tree : Combinator(Tree.T)
tree =
	Combinator.const(|p| |k| { props: p, kids: k })
		.keep(Combinator.const(Dict.empty()))
		.keep(Combinator.const([]))
