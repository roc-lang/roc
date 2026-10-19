//! The compiler's developer-only environment variables, which print internal
//! state or change what a build writes for compiler developers. Every read goes
//! through `get` or `isSet`, which are compile-time null in a compiler built
//! without developer diagnostics, so a release compiler neither reads these
//! variables nor carries the code they enable.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

/// Whether this compiler reads the developer-diagnostic variables at all: it
/// was built with developer diagnostics (every Debug compiler, and any other
/// build made with `-Ddev-diagnostics`) and links libc, which provides the
/// environment.
pub const available = builtin.link_libc and build_options.dev_diagnostics;

/// One developer-diagnostic environment variable.
pub const Var = enum {
    /// Print the final LIR of every procedure whose debug name contains the value.
    lir_dump,
    /// Print every closed specialization key.
    spec_census,
    /// Print every object cache lookup and offer.
    pack_trace,
    /// Print how many procedures a dev build served from cached packs.
    pack_stats,
    /// Print every compile-time demand decision.
    ctfe_demand_trace,
    /// Write one object and one manifest per module next to a native dev build.
    dev_pack_objects,
    /// Load the pack objects in the named directory into a native dev build.
    dev_pack_hits,
    /// Check every object compile against one assembled from its artifacts.
    dev_artifact_roundtrip,
    /// Append one record to the named file per in-process LLVM object compile.
    test_llvm_object_compile_count_file,
    /// `off` never starts a dev build's tier 2 object build, and `wait` waits
    /// for it before the dev build returns.
    background_objects,

    /// The variable's name in the environment.
    pub fn envName(comptime self: Var) [:0]const u8 {
        return switch (self) {
            .lir_dump => "ROC_LIR_DUMP",
            .spec_census => "ROC_SPEC_CENSUS",
            .pack_trace => "ROC_PACK_TRACE",
            .pack_stats => "ROC_PACK_STATS",
            .ctfe_demand_trace => "ROC_CTFE_DEMAND_TRACE",
            .dev_pack_objects => "ROC_DEV_PACK_OBJECTS",
            .dev_pack_hits => "ROC_DEV_PACK_HITS",
            .dev_artifact_roundtrip => "ROC_DEV_ARTIFACT_ROUNDTRIP",
            .test_llvm_object_compile_count_file => "ROC_TEST_LLVM_OBJECT_COMPILE_COUNT_FILE",
            .background_objects => "ROC_BACKGROUND_OBJECTS",
        };
    }
};

/// The variable's value, or null when it is unset or this compiler does not
/// read developer diagnostics.
pub inline fn get(comptime variable: Var) ?[:0]const u8 {
    if (comptime !available) return null;
    return std.mem.span(Cached(variable).read() orelse return null);
}

/// Whether the variable is set. Always false in a compiler that does not read
/// developer diagnostics, so the code it guards is not compiled in.
pub inline fn isSet(comptime variable: Var) bool {
    if (comptime !available) return false;
    return Cached(variable).read() != null;
}

/// Each variable is read from the environment once per process: the compiler
/// never changes its own environment, so every later read would see the same
/// value, and checks on hot paths cost a load instead of an environment scan.
fn Cached(comptime variable: Var) type {
    return struct {
        var loaded: std.atomic.Value(bool) = .init(false);
        var value: std.atomic.Value(?[*:0]const u8) = .init(null);

        fn read() ?[*:0]const u8 {
            if (!loaded.load(.acquire)) {
                // Threads that race here all store the same value.
                value.store(std.c.getenv(variable.envName()), .monotonic);
                loaded.store(true, .release);
            }
            return value.load(.monotonic);
        }
    };
}
