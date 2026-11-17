app [main!] { pf: platform "./static-lib-platform/main.roc" }

# Str.from_utf8 with an ill-formed sequence, and Str.from_utf16_le with an
# unpaired surrogate or a surrogate pair, at every position of inputs long
# enough to cross the 16- and 32-byte blocks a vector validator checks. The
# problems, byte indexes and lossy output must match the byte-range and
# unit-by-unit references written here, so a wasm32 program built with simd128
# reports what a wasm32v1 program built without it reports.
main! = |seed| {
	if utf8_mismatches(seed) == 0 and utf16_mismatches(seed) == 0 {
		"ok"
	} else {
		"mismatch"
	}
}

utf8_mismatches : U64 -> U64
utf8_mismatches = |seed| {
	templates = [
		[0x80],
		[0xFF],
		[0xC0, 0x80],
		[0xC2, 0x41],
		[0xC2],
		[0xE0, 0x80, 0x80],
		[0xED, 0xA0, 0x80],
		[0xE2, 0x82],
		[0xF4, 0x90, 0x80, 0x80],
		[0xF0, 0x9F, 0x98],
	]
	var $mismatches = 0.U64
	for background in ["a", "é", "🎉"] {
		var $len = seed
		while $len <= 36 {
			if !check_utf8(fill(background, $len)) {
				$mismatches = $mismatches + 1
			}
			for template in templates {
				template_len = List.len(template)
				var $pos = 0.U64
				while $pos + template_len <= $len {
					bytes = fill(background, $pos).concat(template).concat(fill(background, $len - $pos - template_len))
					if !check_utf8(bytes) {
						$mismatches = $mismatches + 1
					}
					$pos = $pos + 1
				}
			}
			$len = $len + 1
		}
	}
	$mismatches
}

# Well-formed text of exactly `len` bytes: copies of `background`, padded with
# ASCII where the next copy does not fit.
fill : Str, U64 -> List(U8)
fill = |background, len| {
	unit = background.to_utf8()
	var $out = List.with_capacity(len)
	while List.len($out) + List.len(unit) <= len {
		$out = $out.concat(unit)
	}
	while List.len($out) < len {
		$out = $out.append(0x61)
	}
	$out
}

check_utf8 : List(U8) -> Bool
check_utf8 = |bytes| {
	expected = utf8_reference(bytes)
	match Str.from_utf8(bytes) {
		Ok(text) => expected.code == 0 and text.to_utf8() == bytes
		Err(BadUtf8({ problem, index })) => expected.code != 0 and index == expected.index and problem == utf8_problem_for(expected.code)
	}
}

utf8_problem_for : U8 -> Str.Utf8Problem
utf8_problem_for = |code| {
	match code {
		1 => InvalidStartByte
		2 => UnexpectedEndOfSequence
		3 => ExpectedContinuation
		4 => OverlongEncoding
		5 => EncodesSurrogateHalf
		_ => CodepointTooLarge
	}
}

# The first ill-formed sequence by byte ranges: code 0 for none, otherwise its
# problem (see utf8_problem_for) and start index.
utf8_reference : List(U8) -> { code : U8, index : U64 }
utf8_reference = |bytes| {
	n = List.len(bytes)
	var $i = 0.U64
	while $i < n {
		lead = bytes.get($i) ?? 0
		len = if lead < 0x80 {
			1
		} else if lead >= 0xC0 and lead <= 0xDF {
			2
		} else if lead >= 0xE0 and lead <= 0xEF {
			3
		} else if lead >= 0xF0 and lead <= 0xF7 {
			4
		} else {
			0
		}
		if len == 0 {
			return { code: 1, index: $i }
		}
		if $i + len > n {
			return { code: 2, index: $i }
		}
		var $k = 1.U64
		while $k < len {
			if (bytes.get($i + $k) ?? 0).bitwise_and(0xC0) != 0x80 {
				return { code: 3, index: $i }
			}
			$k = $k + 1
		}
		second = bytes.get($i + 1) ?? 0
		code = if len == 1 {
			0
		} else if lead == 0xC0 or lead == 0xC1 or (lead == 0xE0 and second < 0xA0) or (lead == 0xF0 and second < 0x90) {
			4
		} else if lead == 0xED and second >= 0xA0 {
			5
		} else if (lead == 0xF4 and second >= 0x90) or lead >= 0xF5 {
			6
		} else {
			0
		}
		if code != 0 {
			return { code, index: $i }
		}
		$i = $i + len
	}
	{ code: 0, index: 0 }
}

utf16_mismatches : U64 -> U64
utf16_mismatches = |seed| {
	var $mismatches = 0.U64
	for background in [0x61.U16, 0x4E2D] {
		var $len = seed
		while $len <= 18 {
			var $pos = 0.U64
			while $pos <= $len {
				for injected in [[0xD800.U16], [0xDC00], [0xD83D, 0xDC26, 0xD83D, 0xDC26, 0xD800]] {
					units = List.repeat(background, $pos).concat(injected).concat(List.repeat(background, $len - $pos))
					if !check_utf16(units) {
						$mismatches = $mismatches + 1
					}
				}
				$pos = $pos + 1
			}
			$len = $len + 1
		}
	}
	$mismatches
}

check_utf16 : List(U16) -> Bool
check_utf16 = |units| {
	var $bytes = List.with_capacity(List.len(units) * 2)
	for unit in units {
		$bytes = $bytes.append(unit.to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
	}
	expected = utf16_reference(units)
	strict_ok = match Str.from_utf16_le($bytes) {
		Ok(text) => expected.code == 0 and text.to_utf8() == expected.utf8
		Err(BadUtf16({ problem, index })) => expected.code != 0 and index == expected.index and problem == utf16_problem_for(expected.code)
	}
	strict_ok and Str.from_utf16_le_lossy($bytes).to_utf8() == expected.utf8
}

utf16_problem_for : U8 -> Str.Utf16Problem
utf16_problem_for = |code| {
	match code {
		1 => UnpairedHighSurrogate
		_ => UnpairedLowSurrogate
	}
}

# Lossy UTF-8 output, and the first error: code 0 for none, 1 for an unpaired
# high surrogate, 2 for an unpaired low surrogate.
utf16_reference : List(U16) -> { utf8 : List(U8), code : U8, index : U64 }
utf16_reference = |units| {
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
