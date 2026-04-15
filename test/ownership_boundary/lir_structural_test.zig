//! Scaffolding for structural LIR ownership assertions.
//!
//! This suite will later verify that ownership-sensitive source programs lower
//! to statement-only LIR with explicit `incref` / `decref` / `free` statements
//! and without backend-side ownership reconstruction.

const std = @import("std");

test "structural LIR ownership scaffold" {
    _ = std.testing;
}

