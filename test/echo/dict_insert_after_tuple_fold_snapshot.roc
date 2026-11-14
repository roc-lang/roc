# A dict that a fold's tuple state holds and that one step also appends to the
# state's list keeps its entries when a later step inserts into the state's
# dict (https://github.com/roc-lang/roc/issues/13031).
step : (Dict(U16, U8), List(Dict(U16, U8))), (U16, U8) -> (Dict(U16, U8), List(Dict(U16, U8)))
step = |(d, kept), (k, v)|
	if v == 1 {
		(d, List.append(kept, d))
	} else {
		(Dict.insert(d, k, v), kept)
	}

main! = |args| {
	key = 256 + U64.to_u16_wrap(List.len(args))
	(d, kept) = List.fold([(key, 9.U8), (key, 1), (key, 0)], (Dict.empty(), []), step)
	for k in kept {
		echo!("${Str.inspect(Dict.to_list(k))}\n")
	}
	echo!("${Str.inspect(Dict.to_list(d))}\n")
	Ok({})
}
