# repro for https://github.com/roc-lang/roc/issues/9971
# An unannotated polymorphic function whose body uses == must build and run
# when instantiated at two different types.
is_eq = |a, b| a == b

expect is_eq(1.U8, 1.U8)
expect is_eq("a", "a")
expect !is_eq(1.U8, 2.U8)
expect !is_eq("a", "b")

main! = |_args| {
    x = is_eq(1.U8, 1.U8)
    y = is_eq("a", "a")
    _ = x and y
    Ok({})
}
