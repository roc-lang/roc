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
    /// Trace Boxy-only work; specialized compilation must produce no events.
    trace_boxy_work,
    /// Measure serialization of completed specialization bodies.
    sealed_body_profile,
    /// Exercise validated completed specialization-body replay.
    completed_body_cache,
    /// Separate coordinator request-resolution costs into exclusive phases.
    spec_resolution_profile,
    /// Print the final LIR of every procedure whose debug name contains the value.
    lir_dump,
    /// Print every closed specialization key.
    spec_census,
    /// Print deterministic Lambda Solved work once per required invocation.
    lambda_solve_stats,
    /// Exercise the experimental complete Lambda Solved analysis cache.
    lambda_analysis_cache,
    /// Start each top-level recursive type analysis with empty proof tables.
    fresh_type_analysis,
    /// Print every object cache lookup and offer.
    pack_trace,
    /// Print how many procedures a dev build served from cached packs.
    pack_stats,
    /// Measure specialization with persistent interface replay disabled.
    disable_persistent_interfaces,
    /// Print every compile-time demand decision.
    ctfe_demand_trace,
    /// Print what each module's compile-time evaluation reused, verified, and stored.
    ctfe_reuse_trace,
    /// Write one object and one manifest per module next to a native dev build.
    dev_pack_objects,
    /// Load the pack objects in the named directory into a native dev build.
    dev_pack_hits,
    /// Check every object compile against one assembled from its artifacts.
    dev_excerpt_roundtrip,
    /// Append one record to the named file per in-process LLVM object compile.
    test_llvm_object_compile_count_file,
    /// Print deterministic work counters for one test command.
    test_command_work,

    /// The variable's name in the environment.
    pub fn envName(comptime self: Var) [:0]const u8 {
        return switch (self) {
            .sealed_body_profile => "ROC_SEALED_BODY_PROFILE",
            .completed_body_cache => "ROC_COMPLETED_BODY_CACHE",
            .spec_resolution_profile => "ROC_SPEC_RESOLUTION_PROFILE",
            .trace_boxy_work => "ROC_TRACE_BOXY_WORK",
            .lir_dump => "ROC_LIR_DUMP",
            .spec_census => "ROC_SPEC_CENSUS",
            .lambda_solve_stats => "ROC_LAMBDA_SOLVE_STATS",
            .lambda_analysis_cache => "ROC_LAMBDA_ANALYSIS_CACHE",
            .fresh_type_analysis => "ROC_FRESH_TYPE_ANALYSIS",
            .pack_trace => "ROC_PACK_TRACE",
            .pack_stats => "ROC_PACK_STATS",
            .disable_persistent_interfaces => "ROC_DISABLE_PERSISTENT_INTERFACES",
            .ctfe_demand_trace => "ROC_CTFE_DEMAND_TRACE",
            .ctfe_reuse_trace => "ROC_CTFE_REUSE_TRACE",
            .dev_pack_objects => "ROC_DEV_PACK_OBJECTS",
            .dev_pack_hits => "ROC_DEV_PACK_HITS",
            .dev_excerpt_roundtrip => "ROC_DEV_EXCERPT_ROUNDTRIP",
            .test_llvm_object_compile_count_file => "ROC_TEST_LLVM_OBJECT_COMPILE_COUNT_FILE",
            .test_command_work => "ROC_TEST_COMMAND_WORK",
        };
    }
};

/// Stage-owned counters used to prove complete-command reuse skips producers.
pub const TestWork = enum {
    specialization,
    ctfe_execution,
    second_specialization,
    runtime_lir,
    arc,
    static_data,
    native_test_procedures,
    test_root_execution,
};
var test_work: [@typeInfo(TestWork).@"enum".field_names.len]std.atomic.Value(u64) = @splat(.init(0));

/// Add `amount` to a test-work counter when test command work tracing is on.
pub fn countTestWork(counter: TestWork, amount: usize) void {
    if (!isSet(.test_command_work)) return;
    _ = test_work[@backingInt(counter)].fetchAdd(amount, .monotonic);
}

/// The number of times a stage has counted this test work.
pub fn testWorkCount(counter: TestWork) u64 {
    return test_work[@backingInt(counter)].load(.monotonic);
}

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

/// Called only by owners of Boxy-only work, never by the specialized path.
pub fn traceBoxyWork(comptime activity: []const u8) void {
    if (isSet(.trace_boxy_work)) std.debug.print("boxy-work: " ++ activity ++ "\n", .{});
}
