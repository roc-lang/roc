main! = |args| {
    arg = match args {
        [first, ..] => first
        [] => ""
    }

    for char in arg {
        echo!("arg: ${char}")
    }

    Ok({})
}
