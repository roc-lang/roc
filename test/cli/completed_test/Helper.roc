module [value, positive]

import "input.txt" as input : Str

value = {
    dbg "compile-time observation"
    Str.count_utf8_bytes(input)
}

positive = |n| {
    expect n > 0
    n > 0
}

expect positive(1)
