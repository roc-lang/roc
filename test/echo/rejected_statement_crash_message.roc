# The `+` on `Str` is a type error, so the `line` declaration's value is a
# checked runtime error. Reaching it crashes with the checked-error message.
main! = |_args| {
    echo!("before")
    n = 5.U32
    line = n.to_str() + "\n"
    echo!(line)
    Ok({})
}
