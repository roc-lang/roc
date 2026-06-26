r = {
    number: 1,
    double: |x| x + x,
}

result = r.double(r.number)

main! = |_| {
    Ok({})
}
