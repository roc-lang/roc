main! = |args| {
    arg_count = List.len(args)
    if arg_count + 1 > arg_count {
        Ok({})
    } else {
        Err(Exit(1))
    }
}
