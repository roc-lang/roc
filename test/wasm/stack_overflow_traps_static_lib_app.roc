app [main!] { pf: platform "./static-lib-platform/main.roc" }

## Each level keeps a string live across its non-tail recursive call, and the
## string builtins take their operands by address, so every level holds a
## frame on the shadow stack. The base case is unreachable at runtime, so the
## recursion must exhaust the stack.
descend : U64 -> U64
descend = |n| {
    if n == 0 {
        0
    } else {
        label = "${n.to_str()}-frame"
        below = descend(n + 1)
        below + Str.count_utf8_bytes(label)
    }
}

main! = |seed| {
    depth = descend(seed + 1)

    if depth == 0 {
        "unreachable"
    } else {
        "returned ${depth.to_str()}"
    }
}
