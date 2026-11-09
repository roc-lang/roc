# A branch of an `if` or `match` in tail position whose loop reassigns
# `var`s declared before the construct. The branch after it reads those
# `var`s as they were before the construct, not as the loop left them.
flush : List(U8), U64, U64, Bool -> List(U8)
flush = |out_0, count_0, limit, stored| {
	var $out = out_0
	var $count = count_0
	if stored {
		while $count < limit {
			$out = List.append($out, 1)
			$count = $count + 1
		}
		$out
	} else {
		List.append($out, $count.to_u8_wrap())
	}
}

select : List(U8), U64, U64, Bool -> List(U8)
select = |out_0, count_0, limit, stored| {
	var $out = out_0
	var $count = count_0
	match stored {
		Bool.True => {
			while $count < limit {
				$out = List.append($out, 1)
				$count = $count + 1
			}
			$out
		}
		Bool.False => List.append($out, $count.to_u8_wrap())
	}
}

main! = |args| {
	n = List.len(args) + 1
	seed = List.repeat(9, n)
	echo!("${Str.inspect(flush(seed, 0, 3, Bool.True))} ${Str.inspect(flush(seed, 5, 3, Bool.False))} ${Str.inspect(select(seed, 0, 4, Bool.True))} ${Str.inspect(select(seed, 5, 3, Bool.False))}")
	Ok({})
}
