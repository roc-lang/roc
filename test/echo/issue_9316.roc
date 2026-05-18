main! = |_args| {
    acc0 : List(Str)
    acc0 = []
    acc1 = acc0.concat(["BAZ", "DUCK"])
    # This second concat causes SIGABRT:
    acc2 = acc1.concat(["XYZ", "ABC"])
    echo!("${Str.inspect(acc2)}")

    Ok({})
}
