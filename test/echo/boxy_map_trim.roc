main! = |_args| {
    names = ["  Alice ", "Bob  ", " Charlie"]
    trimmed = names.map(|name| name.trim())
    echo!("${Str.join_with(trimmed, ", ")}\n")
    Ok({})
}
