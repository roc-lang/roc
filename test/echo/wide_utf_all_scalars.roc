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
	bytes16le = utf16_le_bytes($utf16)
	bytes16be = utf16_be_bytes($utf16)
	bytes32le = utf32_le_bytes($utf32)
	bytes32be = utf32_be_bytes($utf32)
	valid = Crypto.SHA256.hash(Str.from_utf16_le(bytes16le).ok_or("").to_utf8()).to_hex() == expected and
		Crypto.SHA256.hash(Str.from_utf16_le_lossy(bytes16le).to_utf8()).to_hex() == expected and
			Crypto.SHA256.hash(Str.from_utf16_bom([255, 254].concat(bytes16le)).ok_or("").to_utf8()).to_hex() == expected and
				Crypto.SHA256.hash(Str.from_utf16_bom_lossy([255, 254].concat(bytes16le)).ok_or("").to_utf8()).to_hex() == expected and
					Crypto.SHA256.hash(Str.from_utf16_be(bytes16be).ok_or("").to_utf8()).to_hex() == expected and
						Crypto.SHA256.hash(Str.from_utf16_be_lossy(bytes16be).to_utf8()).to_hex() == expected and
							Crypto.SHA256.hash(Str.from_utf16_bom([254, 255].concat(bytes16be)).ok_or("").to_utf8()).to_hex() == expected and
								Crypto.SHA256.hash(Str.from_utf16_bom_lossy([254, 255].concat(bytes16be)).ok_or("").to_utf8()).to_hex() == expected and
									Crypto.SHA256.hash(Str.from_utf32_le(bytes32le).ok_or("").to_utf8()).to_hex() == expected and
										Crypto.SHA256.hash(Str.from_utf32_le_lossy(bytes32le).to_utf8()).to_hex() == expected and
											Crypto.SHA256.hash(Str.from_utf32_bom([255, 254, 0, 0].concat(bytes32le)).ok_or("").to_utf8()).to_hex() == expected and
												Crypto.SHA256.hash(Str.from_utf32_bom_lossy([255, 254, 0, 0].concat(bytes32le)).ok_or("").to_utf8()).to_hex() == expected and
													Crypto.SHA256.hash(Str.from_utf32_be(bytes32be).ok_or("").to_utf8()).to_hex() == expected and
														Crypto.SHA256.hash(Str.from_utf32_be_lossy(bytes32be).to_utf8()).to_hex() == expected and
															Crypto.SHA256.hash(Str.from_utf32_bom([0, 0, 254, 255].concat(bytes32be)).ok_or("").to_utf8()).to_hex() == expected and
																Crypto.SHA256.hash(Str.from_utf32_bom_lossy([0, 0, 254, 255].concat(bytes32be)).ok_or("").to_utf8()).to_hex() == expected
	if !valid {
		crash "Wide UTF scalar sweep disagrees with reference UTF-8"
	}
	echo!("ok\n")
	Ok({})
}

utf16_le_bytes : List(U16) -> List(U8)
utf16_le_bytes = |units| {
	var $bytes = List.with_capacity(List.len(units) * 2)
	for unit in units {
		$bytes = $bytes.append(unit.to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
	}
	$bytes
}

utf32_le_bytes : List(U32) -> List(U8)
utf32_le_bytes = |units| {
	var $bytes = List.with_capacity(List.len(units) * 4)
	for unit in units {
		$bytes = $bytes.append(unit.to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(16).to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(24).to_u8_wrap())
	}
	$bytes
}

utf16_be_bytes : List(U16) -> List(U8)
utf16_be_bytes = |units| {
	var $bytes = List.with_capacity(List.len(units) * 2)
	for unit in units {
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
		$bytes = $bytes.append(unit.to_u8_wrap())
	}
	$bytes
}

utf32_be_bytes : List(U32) -> List(U8)
utf32_be_bytes = |units| {
	var $bytes = List.with_capacity(List.len(units) * 4)
	for unit in units {
		$bytes = $bytes.append(unit.shr_wrap(24).to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(16).to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
		$bytes = $bytes.append(unit.to_u8_wrap())
	}
	$bytes
}
