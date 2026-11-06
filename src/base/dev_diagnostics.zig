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
    /// Rebuild every restored Boxy object and compare it with the cached one.
    verify_boxy_object_reuse,
    /// Separate coordinator request-resolution costs into exclusive phases.
    spec_resolution_profile,
    /// Print the final LIR of every procedure whose debug name contains the value.
    lir_dump,
    /// Print every closed specialization key.
    spec_census,
    /// Print deterministic Lambda Solved work once per required invocation.
    lambda_solve_stats,
    /// Start each top-level recursive type analysis with empty proof tables.
    fresh_type_analysis,
    /// Print every compile-time demand decision.
    ctfe_demand_trace,
    /// Check every object compile against one assembled from its artifacts.
    dev_excerpt_roundtrip,
    /// Append one record to the named file per in-process LLVM object compile.
    test_llvm_object_compile_count_file,
    /// Run the independent ARC certifier in a diagnostics-enabled compiler.
    verify_arc,
    /// Print deterministic work counters for one test command.
    test_command_work,
    /// Aggregate test allocations by their first sixteen native stack frames.
    profile_allocations,
    /// Write the bytes of a dev run's first run image, as published and
    /// before the shim loads it, to the named file.
    dump_run_image,
    /// Check that the sealed type view renders every published source type
    /// root exactly as checked type publication does.
    verify_sealed_type_view,
    /// Emit every wasm procedure whose body passes this many bytes as a
    /// family of functions of at most this many bytes of code each, so every
    /// program exercises wasm function splitting.
    wasm_split_bytes,
    /// Write every wasm unit image a wasm build restores or builds into the
    /// named directory, one file per unit named by its unit key.
    dump_wasm_unit_images,

    /// The variable's name in the environment.
    pub fn envName(comptime self: Var) [:0]const u8 {
        return switch (self) {
            .spec_resolution_profile => "ROC_SPEC_RESOLUTION_PROFILE",
            .trace_boxy_work => "ROC_TRACE_BOXY_WORK",
            .verify_boxy_object_reuse => "ROC_VERIFY_BOXY_OBJECT_REUSE",
            .lir_dump => "ROC_LIR_DUMP",
            .spec_census => "ROC_SPEC_CENSUS",
            .lambda_solve_stats => "ROC_LAMBDA_SOLVE_STATS",
            .fresh_type_analysis => "ROC_FRESH_TYPE_ANALYSIS",
            .ctfe_demand_trace => "ROC_CTFE_DEMAND_TRACE",
            .dev_excerpt_roundtrip => "ROC_DEV_EXCERPT_ROUNDTRIP",
            .test_llvm_object_compile_count_file => "ROC_TEST_LLVM_OBJECT_COMPILE_COUNT_FILE",
            .verify_arc => "ROC_VERIFY_ARC",
            .test_command_work => "ROC_TEST_COMMAND_WORK",
            .profile_allocations => "ROC_PROFILE_ALLOCATIONS",
            .dump_run_image => "ROC_DUMP_RUN_IMAGE",
            .verify_sealed_type_view => "ROC_VERIFY_SEALED_TYPE_VIEW",
            .wasm_split_bytes => "ROC_WASM_SPLIT_BYTES",
            .dump_wasm_unit_images => "ROC_DUMP_WASM_UNIT_IMAGES",
        };
    }
};

/// Stage-owned counters used to prove complete-command reuse skips producers.
pub const TestWork = enum {
    checked_root_eligibility_queries,
    checked_root_concreteness_nodes,
    checked_root_callable_nodes,
    checked_type_digest_requests,
    checked_type_digest_walks,
    checked_type_digest_nodes,
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
