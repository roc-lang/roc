# An annotation cannot make a value that is not a function polymorphic. Each
# rejected annotation is reported once, even where its binding is used at two
# types, and the program runs until it reaches a use of a rejected binding,
# which crashes.
empty : List(a)
empty = []

mk = |{}| |x| x

id : a -> a
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
