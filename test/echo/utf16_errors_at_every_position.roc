# UTF-16 decoding with an unpaired high surrogate, an unpaired low surrogate, a
# run of valid surrogate pairs, or pairs followed by an unpaired high surrogate
# at every position of inputs up to 18 code units, with and without an odd
# trailing byte, over ASCII and 3-byte text. Errors and pairs fall before,
# inside, after and across each sixteen-byte block. Strict decoding must
# report the first error's kind and byte index, and lossy decoding must match
# a unit-by-unit reference decoder written here.
main! = |_| {
	var $checked = 0.U64
	for background in [0x61.U16, 0x4E2D] {
		var $len = 0.U64
		while $len <= 18 {
			var $pos = 0.U64
			while $pos <= $len {
				for injected in [[0xD800.U16], [0xDC00], [0xD83D, 0xDC26], [0xD83D, 0xDC26, 0xD83D, 0xDC26, 0xD83D, 0xDC26, 0xD83D, 0xDC26], [0xD83D, 0xDC26, 0xD83D, 0xDC26, 0xD800]] {
					units = List.repeat(background, $pos).concat(injected).concat(List.repeat(background, $len - $pos))
					for odd_tail in [False, True] {
						if !check(units, odd_tail) {
							crash "UTF-16 decoding disagrees with the reference"
						}
						$checked = $checked + 1
					}
				}
				$pos = $pos + 1
			}
			$len = $len + 1
		}
	}
	echo!("ok ${$checked.to_str()}\n")
	Ok({})
}

check : List(U16), Bool -> Bool
check = |units, odd_tail| {
	var $bytes = List.with_capacity(List.len(units) * 2 + 1)
	for unit in units {
		$bytes = $bytes.append(unit.to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
	}
	if odd_tail {
		$bytes = $bytes.append(0x41)
	}
	expected = reference(units, odd_tail)
	strict_ok = match Str.from_utf16_le($bytes) {
		Ok(text) => expected.code == 0 and text.to_utf8() == expected.utf8
		Err(BadUtf16({ problem, index })) => expected.code != 0 and index == expected.index and problem == problem_for(expected.code)
	}
	strict_ok and Str.from_utf16_le_lossy($bytes).to_utf8() == expected.utf8
}

problem_for : U8 -> Str.Utf16Problem
problem_for = |code| {
	match code {
		1 => UnpairedHighSurrogate
		2 => UnpairedLowSurrogate
		_ => UnexpectedEndOfSequence
	}
}

# Lossy UTF-8 output, and the first error: code 0 for none, 1 for an unpaired
# high surrogate, 2 for an unpaired low surrogate, 3 for an odd trailing byte.
reference : List(U16), Bool -> { utf8 : List(U8), code : U8, index : U64 }
reference = |units, odd_tail| {
	n = List.len(units)
	var $out = []
	var $code = 0.U8
	var $index = 0.U64
	var $i = 0.U64
	while $i < n {
		unit = units.get($i) ?? 0
		next = if $i + 1 < n {
			units.get($i + 1) ?? 0
		} else {
			0
		}
		if unit >= 0xD800 and unit <= 0xDBFF and next >= 0xDC00 and next <= 0xDFFF {
			$out = encode_utf8($out, 0x10000 + (unit.to_u32() - 0xD800) * 1024 + (next.to_u32() - 0xDC00))
			$i = $i + 2
		} else if unit >= 0xD800 and unit <= 0xDFFF {
			if $code == 0 {
				$code = if unit <= 0xDBFF {
					1
				} else {
					2
				}
				$index = $i * 2
			}
			$out = encode_utf8($out, 0xFFFD)
			$i = $i + 1
		} else {
			$out = encode_utf8($out, unit.to_u32())
			$i = $i + 1
		}
	}
	if odd_tail {
		if $code == 0 {
			$code = 3
			$index = n * 2
		}
		$out = encode_utf8($out, 0xFFFD)
	}
	{ utf8: $out, code: $code, index: $index }
}

encode_utf8 : List(U8), U32 -> List(U8)
encode_utf8 = |bytes, scalar| {
	if scalar < 0x80 {
		bytes.append(scalar.to_u8_wrap())
	} else if scalar < 0x800 {
		bytes.append(scalar.shr_wrap(6).bitwise_or(0xC0).to_u8_wrap()).append(scalar.bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap())
	} else if scalar < 0x10000 {
		bytes.append(scalar.shr_wrap(12).bitwise_or(0xE0).to_u8_wrap()).append(scalar.shr_wrap(6).bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap()).append(scalar.bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap())
	} else {
		bytes.append(scalar.shr_wrap(18).bitwise_or(0xF0).to_u8_wrap()).append(scalar.shr_wrap(12).bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap()).append(scalar.shr_wrap(6).bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap()).append(scalar.bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap())
	}
}
