app [main!] { pf: platform "./static-lib-platform/main.roc" }

# https://github.com/roc-lang/roc/issues/13017
# The UTF-8 bytes of an empty string are an empty list that owns no
# allocation, so concatenating them leaves every allocation released.
main! = |seed| {
    text = if seed == 0 "" else "author"
    bytes = text.to_utf8()
    out = [34.U8].concat(bytes).append(34)

    if out.len() == 2 "ok" else "bad"
}
