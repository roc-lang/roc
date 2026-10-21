# One field type at two places in a record (`value` and `nested.value`)
# encoded to JSON: both places hold the same descriptor.
main! = |args| {
    value = args.first() ?? "hello"
    echo!(Json.to_str({ nested: { value }, value }))
    Ok({})
}
