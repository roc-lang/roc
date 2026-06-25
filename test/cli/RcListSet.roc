expect {
    elem = ["a"]
    list = [["b"]]
    nested_ok = match List.set(list, 0, elem) {
        Ok(result) => result == [["a"]]
        Err(_) => Bool.False
    }

    tuple_list : List((Str, U32, U32))
    tuple_list = [("left", 1, 10), ("middle", 2, 20)]
    tuple_ok = match List.set(tuple_list, 1, ("new", 9, 90)) {
        Ok(result) =>
            match List.get(result, 1) {
                Ok((label, pitch, duration)) => label == "new" and pitch == 9 and duration == 90
                Err(_) => Bool.False
            }
        Err(_) => Bool.False
    }

    zst_list : List({})
    zst_list = [{}, {}, {}]
    zst_ok = match List.set(zst_list, 1, {}) {
        Ok(result) => List.len(result) == 3
        Err(_) => Bool.False
    }

    nested_ok and tuple_ok and zst_ok
}

main! = |_| {
    Ok({})
}
