main! = |args| {
    text = Str.join_with(List.append(args, "ab"), "")
    count = List.len(args) + 300
    echo!("${Str.inspect(Str.from_utf8(Str.to_utf8(text)))}\n")
    echo!("${Str.inspect(Str.from_utf8([0xFF, 0x41]))}\n")
    echo!("${Str.inspect(U64.to_i32_try(count))}\n")
    echo!("${Str.inspect(U64.to_u8_try(count))}\n")
    echo!("${Str.inspect(I32.from_str("-42"))}\n")
    Ok({})
}
