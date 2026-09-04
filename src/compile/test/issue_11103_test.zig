//! Regression coverage for nominal-backing index fragmentation while
//! specializing generated record-payload codecs.

const std = @import("std");
const postcheck = @import("postcheck");
const harness = @import("lower_to_lir_harness.zig");

test "issue 11103: distinct record payload codecs do not fragment the nominal backing index" {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};

    try harness.expectLowersToLirWithOptions(
        \\U : [
        \\    V0({ f0 : Str }),
        \\    V1({ f1 : Str }),
        \\    V2({ f2 : Str }),
        \\    V3({ f3 : Str }),
        \\    V4({ f4 : Str }),
        \\    V5({ f5 : Str }),
        \\    V6({ f6 : Str }),
        \\    V7({ f7 : Str }),
        \\]
        \\
        \\main! = |args| {
        \\    parsed : Try(U, _)
        \\    parsed = Json.parse(args.get(0) ?? "")
        \\    match parsed {
        \\        Ok(_) => Ok({})
        \\        Err(_) => Err(Exit(1))
        \\    }
        \\}
    , .{
        .monotype_only = true,
        .monotype_diagnostics_out = &diagnostics,
    });

    try std.testing.expectEqual(
        @as(u64, 0),
        diagnostics.graph.nominal_backing_tombstone_deletions,
    );
}
