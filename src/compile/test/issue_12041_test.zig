//! Regression test for https://github.com/roc-lang/roc/issues/12041.
const harness = @import("lower_to_lir_harness.zig");

test "issue 12041: annotated forwarding methods specialize without closing error rows" {
    try harness.expectLowersToLirWithOptions(
        \\
        \\Db(stream) :: { stream : stream }.{
        \\    with! = |db, body!| {
        \\        _ = db.stream.write!()
        \\        body!(db.stream)
        \\    }
        \\
        \\    query! : Db(_) => Try({}, _)
        \\    query! = |db| db.with!(|stream| stream.write!())
        \\}
        \\
        \\Stream := {}.{
        \\    write! : Stream => Try({}, [Offline])
        \\    write! = |_stream| Err(Offline)
        \\}
        \\
        \\handle! : Db(_) => Try({}, _)
        \\handle! = |_db| Ok({})
        \\
        \\with_row! = |db, handler!| handler!(db.query!()?)
        \\
        \\run! = |db| with_row!(db, |{}| handle!(db))
        \\
        \\main! = |_args| {
        \\    _ = run!(Db.{ stream: Stream.{} })
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}

test "issue 12041: omitted evidence preserves independent generic method results" {
    try harness.expectLowersToLir(
        \\Producer :: {}.{
        \\    produce : Producer, a -> a
        \\    produce = |_producer, value| value
        \\}
        \\
        \\forward = |producer| {
        \\    _ = producer.produce("first")
        \\    producer.produce({})
        \\}
        \\
        \\main! = |_args| {
        \\    {} = forward(Producer.{})
        \\    Ok({})
        \\}
    );
}
