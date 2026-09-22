repeat : Try(U64, [Unset]) -> List(Try(U64, [Unset]))
repeat = |value| List.map([0, 1], |_| value)

main! = |_args| {
    echo!("${Str.inspect(repeat(Err(Unset)))}\n")
    echo!("${Str.inspect(repeat(Ok(7)))}\n")
    Ok({})
}
