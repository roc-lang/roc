# Range proving seeds a merge's region with what every edge into it proves.
# An arm entered only when `byte == '\r'` proves the byte is 13 there; that
# must not reach the `else if` after the branches rejoin, where a byte that is
# not 13 also arrives. When it did, optimized builds folded `byte == '\r'` in
# the `else if` to true, so the location of offset 2 in "ab<cd" was wrong.

location_of : List(U8), U64 -> { line : U64, column : U64 }
location_of = |bytes, offset| {
    var $line = 1
    var $start = 0
    var $index = 0
    while $index < offset {
        byte = bytes.get($index) ?? 0
        if byte == '\n' or (byte == '\r' and bytes.get($index + 1) != Ok('\n')) {
            $line = $line + 1
            $start = $index + 1
        } else if byte == '\r' and $index + 1 >= offset {
            $line = $line + 1
            $start = $index + 1
        }
        $index = $index + 1
    }
    { line: $line, column: offset - $start + 1 }
}

main! = |args| {
    bytes = (args.first() ?? "").to_utf8()
    target = { line: 1, column: 3 }
    var $found = False
    var $offset = 0
    while $offset < bytes.len() {
        if location_of(bytes, $offset) == target {
            $found = True
        }
        $offset = $offset + 1
    }
    echo!("found ${Str.inspect($found)}")
    Ok({})
}
