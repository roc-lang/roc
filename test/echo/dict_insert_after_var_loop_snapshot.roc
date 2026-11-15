# A dict that a loop variable carries and that one iteration also appends to
# a list keeps its entries when a later iteration inserts into the loop
# variable (https://github.com/roc-lang/roc/issues/13031).
main! = |args| {
	key = 256 + U64.to_u16_wrap(List.len(args))
	var $d = Dict.insert(Dict.empty(), key, 9.U8)
	var $kept = []
	for step in [5.U8, 1, 0] {
		if step == 1 {
			$kept = List.append($kept, $d)
		} else {
			$d = Dict.insert($d, key, step)
		}
	}
	for k in $kept {
		echo!("${Str.inspect(Dict.to_list(k))}\n")
	}
	echo!("${Str.inspect(Dict.to_list($d))}\n")
	Ok({})
}
