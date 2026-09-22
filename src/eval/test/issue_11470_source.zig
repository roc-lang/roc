//! Shared runtime proof for error-row composition across both lowering strategies.

/// Runtime modes cover success and bare/wrapped callee and callback errors.
pub const source =
    \\find = |query, key| {
    \\    rows = query(key)?
    \\    if rows == 0 { Err(NotFound) } else { Ok(rows) }
    \\}
    \\show = |query| {
    \\    first = find(query, 0.U8)?
    \\    second = find(query, 1.U8) ? Wrapped
    \\    Ok(first + second)
    \\}
    \\run : U64 -> U64
    \\run = |mode| {
    \\    result = show(|key| {
    \\        if (mode == 1 and key == 0) or (mode == 2 and key == 1) {
    \\            Ok(0.U64)
    \\        } else if (mode == 3 and key == 0) or (mode == 4 and key == 1) {
    \\            Err(QueryFailed(Str.concat("an owned error payload longer than inline capacity: ", Str.inspect(mode))))
    \\        } else {
    \\            Ok(20.U64)
    \\        }
    \\    })
    \\    match result {
    \\        Ok(n) => n
    \\        Err(NotFound) => 1
    \\        Err(Wrapped(NotFound)) => 2
    \\        Err(QueryFailed(text)) => if text == "an owned error payload longer than inline capacity: 3" { 3 } else { crash "wrong bare payload" }
    \\        Err(Wrapped(QueryFailed(text))) => if text == "an owned error payload longer than inline capacity: 4" { 4 } else { crash "wrong wrapped payload" }
    \\    }
    \\}
;
