# An `if` whose branches build `Some([...])` and the empty `None`: the
# result is described the same way on both branches.
main! = |args| {
    value = if args.len() > 0 Some(["hello"]) else None
    echo!(Str.inspect(value))
    Ok({})
}
