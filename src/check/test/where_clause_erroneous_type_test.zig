//! A where clause is part of the type its annotation generates, so an
//! annotation whose where clause names a type with no spelling declares no
//! type, and nothing is related to it.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

test "a where clause naming an internal builtin type makes the annotation erroneous" {
    var env = try TestEnv.init("Test",
        \\stamp : r -> Str where [r.encoder_for : Encoding.JsonEncoding -> {}]
        \\stamp = |_| "ok"
        \\
        \\used = stamp({})
    );
    defer env.deinit();
    try env.assertOneCanError("Internal Builtin Type");
    try std.testing.expectEqual(@as(usize, 0), try env.typeProblemCount());
}
