main! = |_| {
    i = 5
    code = match i {
        300 => 7
        _ => i
    }
    Err(Exit(code))
}
