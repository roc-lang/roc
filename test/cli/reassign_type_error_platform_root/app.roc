app [target] { pf: platform "./platform.roc" }

main! : List(U8) => U8
main! = |_data| {
	var $list = List.with_capacity(4)
	$list = List.append($list, 0.U64)
	$list = List.set($list, 0, 5)
	crash "len=${List.len($list).to_str()}"
}

target = { run!: main! }
