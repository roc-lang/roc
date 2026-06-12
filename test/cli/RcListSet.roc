expect {
    elem = ["a"]
    list = [["b"]]
    match List.set(list, 0, elem) {
        Ok(result) => result == [["a"]]
        Err(_) => Bool.False
    }
}

main! = |_| {
    Ok({})
}
