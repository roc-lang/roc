# Fixture for the match-extension codegen check. This is the inner loop shape
# LZ77-style compressors spend most of their time in: compare two positions in
# one byte list, eight bytes at a time, and report how many bytes agree.
#
# It is written the way that loop wants to be written, and each of those choices
# is load-bearing for the generated code:
#
#   - the loop has no condition of its own; `from_le_bytes` reporting the end of
#     the list is what ends it, so one check serves as both the bounds check and
#     the loop's termination test rather than two checks restating each other
#   - the index advances with `plus_wrap`, since checked addition would add an
#     overflow branch to every iteration for an overflow that cannot happen
#   - running out of buffer returns what has matched so far, rather than
#     crashing, because reaching the end is an ordinary outcome here
#
# ci/check_match_extension_codegen.sh pins the instruction counts this produces.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

match_extension : List(U8), U64, U64 -> U64
match_extension = |bytes, a_start, b_start| {
	# Symmetric in the two positions, and the later one runs out of buffer
	# first, so walk from the earlier and let the later one be the bound.
	lo = a_start.min(b_start)
	delta = a_start.max(b_start).minus_saturated(lo)
	var $acc = 0.U64
	var $a = lo

	while True {
		# Check the later position first so its bounds check governs both reads.
		y = match U64.from_le_bytes(bytes, $a.plus_wrap(delta)) {
			Ok(v) => v
			Err(_) => {
				break
			}
		}
		x = match U64.from_le_bytes(bytes, $a) {
			Ok(v) => v
			Err(_) => {
				break
			}
		}
		if x != y {
			# First differing byte: first differing bit, rounded down.
			return $acc.plus_wrap(U64.count_trailing_zero_bits(x.bitwise_xor(y)).to_u64() // 8)
		}
		$acc = $acc.plus_wrap(8)
		$a = $a.plus_wrap(8)
	}

	# Fewer than eight bytes left at one of the positions; finish by byte.
	while True {
		q = match bytes.get($a.plus_wrap(delta)) {
			Ok(v) => v
			Err(_) => {
				break
			}
		}
		p = match bytes.get($a) {
			Ok(v) => v
			Err(_) => {
				break
			}
		}
		if p != q {
			break
		}
		$acc = $acc.plus_wrap(1)
		$a = $a.plus_wrap(1)
	}

	$acc
}

main! = |args| {
	# Length and contents both come from the command line so nothing about the
	# buffer is known at compile time.
	n = args.len().to_u64() * 512 + 64
	bytes = List.repeat(args.len().to_u8_wrap(), n)
	# Consumed by the exit status so the call cannot be optimized away.
	if match_extension(bytes, 0, 1000) > n {
		Err(Exit(1))
	} else {
		Ok({})
	}
}
