r = {
    number: 1,
    double: |x| x + x,
}

result = r.double(r.number)

main! = |_| {
    if result == 2 {
        Ok({})
    } else {
        Err(Exit(1))
    }
}
