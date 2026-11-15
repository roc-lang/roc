app [main!] { pf: platform "./static-lib-platform/main.roc" }

# Str.to_utf8 of every string shape: the empty list it gives an empty string
# owns no allocation and has no capacity, a small string's bytes are copied,
# and a heap string's bytes (whole or a seamless slice) are shared.
describe : List(U8) -> Str
describe = |bytes| "${bytes.len().to_str()}/${bytes.capacity().to_str()}"

main! = |seed| {
    empty_small = if seed == 0 "" else "x"
    empty_heap = Str.with_capacity(40 + seed)
    small = if seed == 0 "abc" else "xyz"
    heap = if seed == 0 "a string long enough to live on the heap" else "short"
    sliced = heap.drop_prefix("a string ")

    parts = [
        describe(empty_small.to_utf8()),
        describe(empty_heap.to_utf8()),
        describe(small.to_utf8()),
        heap.to_utf8().len().to_str(),
        sliced.to_utf8().len().to_str(),
        Str.from_utf8(sliced.to_utf8()) ?? "bad",
        Str.from_utf8(small.to_utf8()) ?? "bad",
    ]

    joined = Str.join_with(parts, " ")
    if joined == "0/0 0/0 3/3 40 31 long enough to live on the heap abc" "ok" else joined
}
