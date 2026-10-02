# repro for https://github.com/roc-lang/roc/issues/11851
#
# Inspecting a string escapes only quotes and backslashes, in every lowering
# strategy, including when the string is reached through a generic value.
wrap = |x| Str.inspect(x)

main! = |_| {
    echo!("${Str.inspect("line1\nline2 \"q\" back\\slash")}\n")
    echo!("${wrap({ s: "a\tb", l: ["c\nd"] })}\n")
    Ok({})
}
