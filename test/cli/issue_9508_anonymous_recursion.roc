main! = |_args| {
    h = |x| {
        match x {
            A(a) => a
            _ => x
        }
    }

    Ok({})
}
