foo : {} -> {} where [a.decode : {} -> {}]
foo = |_| {
    A : a
    A.decode({})
}

main! = |_| {
    _ = foo({})
    Ok({})
}
