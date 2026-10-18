app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A loop whose only operation on its list is `append_le_bytes_to` is promoted:
# each word-sized append runs unchecked under a fill limit, and the list grows
# on the checked path. The bytes must match a byte-at-a-time construction for
# every count, starting from a capacity too small for even one word.

pack : U64, U8 -> List(U8)
pack = |n, count| {
	var $out = List.with_capacity(3)
	var $i = 0.U64
	while $i < n {
		word = $i.times_wrap(0x0101010101010101).bitwise_xor(0x5A)
		$out = match word.append_le_bytes_to($out, count) {
			Ok(next) => next
			Err(_) => crash "count is at most 8"
		}
		$i = $i + 1
	}
	$out
}

pack_bytewise : U64, U8 -> List(U8)
pack_bytewise = |n, count| {
	var $out = List.with_capacity(3)
	var $i = 0.U64
	while $i < n {
		word = $i.times_wrap(0x0101010101010101).bitwise_xor(0x5A)
		var $k = 0.U8
		while $k < count {
			$out = List.append($out, word.shr_zf_wrap($k.times_wrap(8)).to_u8_wrap())
			$k = $k + 1
		}
		$i = $i + 1
	}
	$out
}

main! = || {
	var $count = 1.U8
	while $count <= 8 {
		fast = pack(300, $count)
		slow = pack_bytewise(300, $count)
		verdict = if fast == slow { "same" } else { "DIFFERENT" }
		Stdout.line!("count ${$count.to_str()}: len ${fast.len().to_str()} ${verdict}")
		$count = $count + 1
	}
}
