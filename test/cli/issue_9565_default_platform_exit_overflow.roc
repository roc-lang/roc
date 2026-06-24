main! = |_| {
    var i = 0

    while i < 1000000 {
        echo!(i.to_str())
        i = i + 1
    }

    Err(Exit(i))
}
