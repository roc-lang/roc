main! = |_args| {
    echo!("hi\n")
    x = 1.5
    y = 2.25
    Ok(total) = add(x, y)
    echo!("${total.to_str()}\n")
    Ok({})
}

add = |a, b|
    Try.Ok(a + b)
