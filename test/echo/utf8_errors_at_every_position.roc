# Str.from_utf8 with an ill-formed sequence at every position of inputs up to
# 36 bytes, over ASCII and 2- and 4-byte text, so the error falls before,
# inside, after and across each 16- and 32-byte block that a vector validator
# checks, and in the tail it checks last. The reported problem and byte index
# must match a byte-range reference written here, and valid inputs must
# decode to themselves.
main! = |_| {
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
	var $checked = 0.U64
	for background in ["a", "é", "🎉"] {
		var $len = 0.U64
		while $len <= 36 {
			valid = fill(background, $len)
			if !check(valid) {
				crash "Str.from_utf8 rejects valid text"
			}
			for template in templates {
				template_len = List.len(template)
				if template_len <= $len {
					var $pos = 0.U64
					while $pos + template_len <= $len {
						bytes = fill(background, $pos).concat(template).concat(fill(background, $len - $pos - template_len))
						if !check(bytes) {
							crash "Str.from_utf8 disagrees with the reference"
						}
						$checked = $checked + 1
						$pos = $pos + 1
					}
				}
			}
			$len = $len + 1
		}
	}
	echo!("ok ${$checked.to_str()}\n")
	Ok({})
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

check : List(U8) -> Bool
check = |bytes| {
	expected = reference(bytes)
	match Str.from_utf8(bytes) {
		Ok(text) => expected.code == 0 and text.to_utf8() == bytes
		Err(BadUtf8({ problem, index })) => expected.code != 0 and index == expected.index and problem == problem_for(expected.code)
	}
}

problem_for : U8 -> Str.Utf8Problem
problem_for = |code| {
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
# problem (see problem_for) and start index. The lead byte announces the length
# (C0-DF: 2, E0-EF: 3, F0-F7: 4); running past the end comes first, then a
# byte outside 80-BF, then the lead and second byte.
reference : List(U8) -> { code : U8, index : U64 }
reference = |bytes| {
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
