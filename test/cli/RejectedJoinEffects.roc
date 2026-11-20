run = |b| {
    v = if b {
        dbg "chosen branch"
        unknown
    } else {
        dbg "other branch"
        unknown
    }
    v.len()
}

expect run(Bool.True) == 1
