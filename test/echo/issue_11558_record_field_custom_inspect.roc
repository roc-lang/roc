# repro for https://github.com/roc-lang/roc/issues/11558
#
# A record field whose nominal type has a custom `to_inspect` is inspected
# through that method under every backend, including `--specialize=no`.
Color := [Red, Green].{
    to_inspect : Color -> Str
    to_inspect = |c| match c {
        Red => "Color::Red"
        Green => "Color::Green"
    }
}

main! = |args| {
    c : Color
    c = if List.len(args) > 5 { Green } else { Red }
    echo!("${Str.inspect({ color: c, count: 42.I64 })}\n")
    Ok({})
}
