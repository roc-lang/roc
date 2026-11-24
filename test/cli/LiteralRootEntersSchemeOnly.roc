only_runtime_helper = |words| Str.join_with(words, ", ")

# The literal `1` takes its type from the caller, so the caller converts it
# at compile time in this function's scope. Nothing calls the function then.
next_after! = |words, offset| {
    echo!(only_runtime_helper(words))
    offset + 1
}

main! = |_args| {
    total : U64
    total = next_after!(["a", "b"], 41)
    echo!(Str.inspect(total))
    Ok({})
}
