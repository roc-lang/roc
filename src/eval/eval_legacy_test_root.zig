//! Test root that aggregates the legacy eval suites.

test {
    _ = @import("test/RuntimeHostEnv.zig");
    _ = @import("test/TestEnv.zig");
}
