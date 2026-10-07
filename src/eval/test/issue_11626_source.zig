//! Shared regression source for polymorphic `?` return boundaries.

/// Shared callees returned through distinct polymorphic error rows.
pub const source =
    \\find = |text, what| if text == "" Err(NotFound(what)) else Ok(text)
    \\respond = |text| Ok(text)
    \\stats_page = |text| {
    \\    _ = find(text, IndexStats)?
    \\    respond("stats")
    \\}
    \\lead_page = |text| {
    \\    _ = find(text, LeadMissing)?
    \\    respond("lead")
    \\}
    \\text_page = |text| {
    \\    _ = find(text, "owned error payload longer than an inline string")?
    \\    respond("text")
    \\}
    \\number_page = |text| {
    \\    _ = find(text, 7.U64)?
    \\    respond("number")
    \\}
    \\run = |text, body| body(text)
;
