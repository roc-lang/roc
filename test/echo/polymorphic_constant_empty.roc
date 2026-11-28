# A constant whose type keeps a variable that carries no requirement is
# polymorphic: `empty` is used at two item types, `id` holds a function used
# at two types, `hooks` holds a function whose `dbg` renders the value it is
# called with at each type, and `color` is read at two tag unions.
empty : List(_a)
empty = []

mk = |{}| |x| x

id = mk({})

hooks = {
	show: |x| {
		dbg x
		x
	},
}

color = Purple

pastel : [Purple, Mauve] -> Str
pastel = |c|
	match c {
		Purple => "pastel purple"
		Mauve => "mauve"
	}

vibrant : [Purple, Orange] -> Str
vibrant = |c|
	match c {
		Purple => "vibrant purple"
		Orange => "orange"
	}

main! = |_args| {
	bytes = List.append(empty, 1.U8)
	strs = List.append(List.append(empty, "a"), "b")
	echo!("${Str.inspect(List.len(bytes))} ${Str.inspect(List.len(strs))}\n")
	echo!("${id("s")} ${Str.inspect(id(7.U8))}\n")
	_ = (hooks.show)(3.U8)
	_ = (hooks.show)("three")
	echo!("${pastel(color)} ${vibrant(color)}\n")
	Ok({})
}
