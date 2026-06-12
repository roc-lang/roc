expect {
    elem = ["a"]
    list = [["b"]]
    match List.replace(list, 0, elem) {
        Ok({ list: result, prev }) =>
            (result == [["a"]]) and (prev == ["b"])
        Err(_) => Bool.False
    }
}

main! = |_| {
    Ok({})
}
