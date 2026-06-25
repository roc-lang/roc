expect {
    elem = ["a"]
    list = []
    prepended = List.prepend(list, elem)
    prepended == [["a"]]
}

main! = |_| {
    Ok({})
}
