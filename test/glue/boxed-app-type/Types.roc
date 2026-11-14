Types := [].{
	## Holds the application's type by value, so its layout is not known
	## until the application chooses that type.
	Wrap(m) := { model : m, n : U64 }

	## Holds the application's type by value in a tag payload.
	Slot(m) := [Empty, Loaded(m)]

	## Holds the application's type by value three aggregates down.
	Deep(m) := { outer : { pair : (Slot(m), U8) }, label : Str }

	## Recursive, with the application's type by value in one variant.
	Tree(m) := [Leaf(m), Node(Box(Tree(m)))]

	## Reaches the application's type only through further indirections, so
	## its own layout is fully known.
	Indirect(m) := { boxed : Box(m), items : List(m), n : U64 }

	## Never mentions the application's type.
	Known := { count : U64, flag : Bool }
}
