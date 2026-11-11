# Overwriting a key in a dict that another binding still holds must copy the
# dict's entries, so the earlier binding keeps its old value.
main! = |args| {
	key = List.len(args)

	before = Dict.insert(Dict.with_capacity(1), key, 2.U8)
	after = Dict.insert(before, key, 3)
	echo!("${Str.inspect(Dict.get(before, key))} ${Str.inspect(Dict.get(after, key))}\n")

	var $built = Dict.with_capacity(1)
	for k in [key] {
		$built = Dict.insert($built, k, 4.U8)
	}
	alias = $built
	$built = Dict.insert($built, key, 5)
	echo!("${Str.inspect(Dict.get(alias, key))} ${Str.inspect(Dict.get($built, key))}")
	Ok({})
}
