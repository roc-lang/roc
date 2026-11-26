# A `dbg` inside a polymorphic constant's definition is reported once, by
# the constant's canonical instance, however many instances the program
# reads; `Str.inspect` and `dbg` of each instance render that instance.
noisy : List(_a)
noisy = {
	dbg "building noisy"
	[]
}

color = Purple

main! = |_args| {
	bytes = List.append(noisy, 1.U8)
	strs = List.append(noisy, "a")
	echo!("${Str.inspect(bytes)} ${Str.inspect(strs)}\n")
	pick : [Purple, Mauve]
	pick = color
	dbg pick
	other : [Purple, Orange]
	other = color
	echo!("${Str.inspect(other)}\n")
	Ok({})
}
