# SHA-256 of every Unicode scalar encoded as UTF-8 in ascending order.
# Reference generated independently with Python's chr(c).encode("utf-8"),
# for c in range(0x110000), excluding 0xD800..0xDFFF.
main! = |_| {
	var $utf16 = List.with_capacity(2160640)
	var $utf32 = List.with_capacity(1112064)
	var $scalar = 0.U32
	while $scalar <= 0x10FFFF {
		if $scalar < 0xD800 or $scalar > 0xDFFF {
			$utf32 = $utf32.append($scalar)
			if $scalar <= 0xFFFF {
				$utf16 = $utf16.append($scalar.to_u16_wrap())
			} else {
				adjusted = $scalar - 0x10000
				$utf16 = $utf16.append((0xD800 + adjusted.shr_wrap(10)).to_u16_wrap())
				$utf16 = $utf16.append((0xDC00 + adjusted.bitwise_and(1023)).to_u16_wrap())
			}
		}
		$scalar = $scalar + 1
	}
	expected = "e0a7693f7362e88827c15e772e55b3490bd983f90711df7f3ef36c2b1ef6847e"
	strict16 = Str.from_utf16($utf16).ok_or("")
	strict32 = Str.from_utf32($utf32).ok_or("")
	lossy16 = Str.from_utf16_lossy($utf16)
	lossy32 = Str.from_utf32_lossy($utf32)
	valid = Crypto.SHA256.hash(strict16.to_utf8()).to_hex() == expected and strict16 == strict32 and strict16 == lossy16 and strict16 == lossy32
	if !valid {
		crash "Wide UTF scalar sweep disagrees with reference UTF-8"
	}
	echo!("ok\n")
	Ok({})
}
