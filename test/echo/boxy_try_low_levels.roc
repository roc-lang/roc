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
    echo!("${Str.inspect(Str.from_utf16(units16) == Ok(expected) and Str.from_utf16_lossy(units16) == expected)}\n")
    echo!("${Str.inspect(Str.from_utf32(units32) == Ok(expected) and Str.from_utf32_lossy(units32) == expected)}\n")
    echo!("${Str.inspect(Str.from_utf16(List.append(units16, 0xD800)) == Err(BadUtf16({ index: count, problem: UnpairedHighSurrogate })))}\n")
    echo!("${Str.inspect(Str.from_utf32(List.append(units32, 0x110000)) == Err(BadUtf32({ index: count, problem: CodePointTooLarge })))}\n")
    Ok({})
}
