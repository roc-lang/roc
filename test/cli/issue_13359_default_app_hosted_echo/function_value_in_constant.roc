log! = |msg| echo!(msg)

count = List.len([log!, log!])

expect count == 2

main! = |_args| {
    echo!(Str.inspect(count))
    Ok({})
}
