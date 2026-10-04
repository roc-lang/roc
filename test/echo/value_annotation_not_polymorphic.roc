# An annotation cannot make a value that is not a function polymorphic. Each
# rejected annotation is reported, its binding is checked as an unannotated
# one, and the program runs until it reaches a rejected binding, which crashes.
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
	} else {
		{}
	}
	echo!("reaching empty\n")
	echo!(Str.inspect(List.len(empty)))
	Ok({})
}
