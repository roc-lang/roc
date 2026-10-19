app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# A bit writer's flush appends the low bytes of a word to a unique list. Under
# `--opt=speed` a list with a spare word of capacity takes an inline store and
# a list that has to grow takes the builtin; both must leave the same bytes as
# `--opt=dev`. The same holds for a reserve that finds its spare already there
# and one that has to grow.

flush_all : List(U8), U64 -> List(U8)
flush_all = |out0, n| {
	var $out = out0
	var $i = 0
	while $i < n {
		value = $i.times_wrap(0x0101_0101_0101_0101).plus_wrap(0x0706_0504_0302_0100)
		count = $i.bitwise_and(7).plus_wrap(1)
		$out = match value.append_le_bytes_to($out, count.to_u8_wrap()) {
			Ok(next) => next
			Err(_) => crash "count is at most 8"
		}
		$i = $i.plus_wrap(1)
	}
	$out
}

reserve_all : List(U8), U64 -> List(U8)
reserve_all = |out0, n| {
	var $out = out0
	var $i = 0
	while $i < n {
		$out = List.reserve($out, $i).append($i.to_u8_wrap())
		$i = $i.plus_wrap(1)
	}
	$out
}

checksum : List(U8) -> U64
checksum = |bytes| List.fold(bytes, 0, |acc, b| acc.times_wrap(31).plus_wrap(b.to_u64()))

main! = || {
	n = Stdin.line!().to_utf8().len()
	flushed = flush_all(List.with_capacity(16), n.times_wrap(10))
	reserved = reserve_all(List.with_capacity(4), n.times_wrap(10))
	Stdout.line!("${flushed.len().to_str()} ${checksum(flushed).to_str()} ${reserved.len().to_str()} ${checksum(reserved).to_str()}")
}
