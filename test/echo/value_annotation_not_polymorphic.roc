# An annotation cannot make a value polymorphic over a variable that carries a
# requirement (a top-level constant's where clause), nor make a local value
# polymorphic. Each rejected annotation is reported once, even where its
# binding is used at two types, and the program runs until it reaches a use of
# a rejected binding, which crashes.
empty : List(a) where [a.to_str : a -> Str]
empty = []

mk = |{}| |x| x

id : a -> a where [a.to_str : a -> Str]
id = mk({})

fresh : {} -> List(a)
fresh = |{}| []

main! = |args| {
	bytes = List.append(fresh({}), 1.U8)
	strs = List.append(fresh({}), "s")
	echo!("${Str.inspect(List.len(bytes))} ${Str.inspect(List.len(strs))}\n")
	if List.len(args) > 100 {
		pick : a -> a
		pick = if List.len(args) > 200 |x| x else |x| x
		echo!(id(pick("unreached")))
		echo!(Str.inspect(id(1.U8)))
		echo!(Str.inspect(List.len(List.append(empty, 1.U8))))
		echo!(Str.inspect(List.len(List.append(empty, "s"))))
	} else {
		{}
	}
	echo!("reaching empty\n")
	echo!(Str.inspect(List.len(empty)))
	Ok({})
}
