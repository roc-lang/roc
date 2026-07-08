# positive control: the #8848 shape — one nesting level shallower than #9827.
# Two distinct error rows via `?`/`return` plus a forward reference, directly
# in main!'s body (no nested closure). Closed by the value-restriction work
# (#8945); must stay clean so the class is covered at both depths.
main! = |args| {
    if List.len(args) > 99 {
        return Err(FirstError)
    }
    if List.len(args) > 999 {
        return Err(SecondError)
    }
    _ = helper({})
    Ok({})
}

helper = |x| Ok(x)
