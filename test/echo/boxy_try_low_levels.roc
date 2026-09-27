main! = |args| {
    text = Str.join_with(List.append(args, "ab"), "")
    count = List.len(args) + 300
    echo!("${Str.inspect(Str.from_utf8(Str.to_utf8(text)))}\n")
    echo!("${Str.inspect(Str.from_utf8([0xFF, 0x41]))}\n")
    echo!("${Str.inspect(U64.to_i32_try(count))}\n")
    echo!("${Str.inspect(U64.to_u8_try(count))}\n")
    echo!("${Str.inspect(I32.from_str("-42"))}\n")
    units16 = List.repeat(65.U16, count)
    units32 = List.repeat(65.U32, count)
    expected = Str.repeat("A", count)
    echo!("${Str.inspect(Str.from_utf16_le(utf16_bytes(units16)) == Ok(expected) and Str.from_utf16_le_lossy(utf16_bytes(units16)) == expected)}\n")
    echo!("${Str.inspect(Str.from_utf32_le(utf32_bytes(units32)) == Ok(expected) and Str.from_utf32_le_lossy(utf32_bytes(units32)) == expected)}\n")
    echo!("${Str.inspect(Str.from_utf16_le(utf16_bytes(List.append(units16, 0xD800))) == Err(BadUtf16({ index: count * 2, problem: UnpairedHighSurrogate })))}\n")
    echo!("${Str.inspect(Str.from_utf32_le(utf32_bytes(List.append(units32, 0x110000))) == Err(BadUtf32({ index: count * 4, problem: CodePointTooLarge })))}\n")
    Ok({})
}

utf16_bytes : List(U16) -> List(U8)
utf16_bytes = |units| {
	var $bytes = List.with_capacity(List.len(units) * 2)
	for unit in units {
		$bytes = $bytes.append(unit.to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
	}
	$bytes
}

utf32_bytes : List(U32) -> List(U8)
utf32_bytes = |units| {
	var $bytes = List.with_capacity(List.len(units) * 4)
	for unit in units {
		$bytes = $bytes.append(unit.to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(8).to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(16).to_u8_wrap())
		$bytes = $bytes.append(unit.shr_wrap(24).to_u8_wrap())
	}
	$bytes
}
