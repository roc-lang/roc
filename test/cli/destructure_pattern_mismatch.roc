# The `{ y }` pattern does not match a record that also has an `x` field.
# That is a type error, and running the program must crash at runtime when it
# reaches the destructure rather than panicking the compiler during lowering.
main! = |_args| {
    echo!("before")
    { y } = { x: 1, y: 2 }
    echo!(Str.inspect(y))
    Ok({})
}
