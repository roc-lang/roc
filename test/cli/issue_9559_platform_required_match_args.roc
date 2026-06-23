main! = |args| {
    match args {
        [1] => Ok({})
        _ => Err(Exit(1))
    }
}
