Color := [Red, Green, Blue].{
    to_inspect : Color -> Str
    to_inspect = |color| match color {
        Red => "Color::Red"
        Green => "Color::Green"
        Blue => "Color::Blue"
    }
}

main! = |_args| {
    red : Color
    red = Red
    Err(CustomError(red))
}
