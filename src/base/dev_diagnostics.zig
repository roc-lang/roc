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
    /// Trace Boxy work: the `.boxy` compile-time evaluator's under either
    /// strategy, and Boxy lowering's. A specialized compilation whose
    /// compile-time roots are all complete produces no events.
    trace_boxy_work,
    /// Print each part of every Boxy module digest, so two traces show
    /// which part an edit changed. Hashing the parts separately costs more
    /// than the digests themselves, so the work trace leaves it out.
    trace_boxy_digests,
    /// Rebuild every restored or embedded Boxy object and compare it with the
    /// cached or embedded one.
    verify_boxy_object_reuse,
    /// Separate coordinator request-resolution costs into exclusive phases.
    spec_resolution_profile,
    /// Print the final LIR of every procedure whose debug name contains the value.
    lir_dump,
    /// Print every closed specialization key.
    spec_census,
    /// Print the procedures each lowered LIR program compiles, one line per
    /// program and one per procedure.
    proc_census,
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
    /// Emit every wasm procedure whose body passes this many bytes as a
    /// family of functions of at most this many bytes of code each, so every
    /// program exercises wasm function splitting.
    wasm_split_bytes,
    /// Write every wasm unit image a wasm build restores or builds into the
    /// named directory, one file per unit named by its unit key.
    dump_wasm_unit_images,
    /// Print deterministic table storage counters (bytes filled, bytes copied
    /// on growth, mappings, reservations, commits, trims, epoch wraps and
    /// relocations) at exit.
    storage_work,
    /// Append `<start> <size> <name>` for every function of each unit image
    /// the compiler loads into its own process (`roc test`'s and compile-time
    /// evaluation's) to `/tmp/perf-<pid>.map`, so `perf` names them: by the
    /// readable name a lowering of the process recorded (`notePerfName`),
    /// otherwise by symbol.
    perf_map,

    /// The variable's name in the environment.
    pub fn envName(comptime self: Var) [:0]const u8 {
        return switch (self) {
            .spec_resolution_profile => "ROC_SPEC_RESOLUTION_PROFILE",
            .trace_boxy_work => "ROC_TRACE_BOXY_WORK",
            .trace_boxy_digests => "ROC_TRACE_BOXY_DIGESTS",
            .verify_boxy_object_reuse => "ROC_VERIFY_BOXY_OBJECT_REUSE",
            .lir_dump => "ROC_LIR_DUMP",
            .spec_census => "ROC_SPEC_CENSUS",
            .proc_census => "ROC_PROC_CENSUS",
            .lambda_solve_stats => "ROC_LAMBDA_SOLVE_STATS",
            .fresh_type_analysis => "ROC_FRESH_TYPE_ANALYSIS",
            .ctfe_demand_trace => "ROC_CTFE_DEMAND_TRACE",
            .dev_excerpt_roundtrip => "ROC_DEV_EXCERPT_ROUNDTRIP",
            .test_llvm_object_compile_count_file => "ROC_TEST_LLVM_OBJECT_COMPILE_COUNT_FILE",
            .verify_arc => "ROC_VERIFY_ARC",
            .test_command_work => "ROC_TEST_COMMAND_WORK",
            .profile_allocations => "ROC_PROFILE_ALLOCATIONS",
            .dump_run_image => "ROC_DUMP_RUN_IMAGE",
            .wasm_split_bytes => "ROC_WASM_SPLIT_BYTES",
            .dump_wasm_unit_images => "ROC_DUMP_WASM_UNIT_IMAGES",
            .storage_work => "ROC_STORAGE_WORK",
            .perf_map => "ROC_PERF_MAP",
        };
    }
};

/// Stage-owned counters used to prove complete-command reuse skips producers.
pub const TestWork = enum {
    ctfe_function_unit_hits,
    ctfe_function_unit_builds,
    ctfe_function_metadata_bytes,
    boxy_body_index_available_modules,
    boxy_body_index_prepared_modules,
    boxy_body_index_rows,
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

/// The readable name of each procedure a lowering of this process named, by
/// the procedure's symbol, which the perf map (`perf_map`) writes in place of
/// the symbol. Lowering and the images the map covers share the process.
const perf_names = struct {
    var mutex: std.atomic.Mutex = .unlocked;
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    var by_symbol: std.StringHashMapUnmanaged([]const u8) = .empty;

    fn lock() void {
        while (!mutex.tryLock()) std.atomic.spinLoopHint();
    }
};

/// Record `name` as the readable name of the procedure whose symbol is
/// `symbol`, when `perf_map` is set. A symbol keeps the first name it gets;
/// a failure to allocate leaves it unnamed.
pub fn notePerfName(symbol: []const u8, name: []const u8) void {
    if (!isSet(.perf_map)) return;
    recordPerfName(symbol, name);
}

fn recordPerfName(symbol: []const u8, name: []const u8) void {
    perf_names.lock();
    defer perf_names.mutex.unlock();
    const a = perf_names.arena.allocator();
    if (perf_names.by_symbol.contains(symbol)) return;
    const owned_symbol = a.dupe(u8, symbol) catch return;
    const owned_name = a.dupe(u8, name) catch return;
    perf_names.by_symbol.put(a, owned_symbol, owned_name) catch return;
}

/// The readable name a lowering recorded for the procedure whose symbol is
/// `symbol`, if any. Only `perf_map` touches the table.
pub fn perfName(symbol: []const u8) ?[]const u8 {
    if (!isSet(.perf_map)) return null;
    return lookupPerfName(symbol);
}

fn lookupPerfName(symbol: []const u8) ?[]const u8 {
    perf_names.lock();
    defer perf_names.mutex.unlock();
    return perf_names.by_symbol.get(symbol);
}

test "a procedure keeps the first readable name recorded for its symbol" {
    recordPerfName("roc__ptest_symbol_one", "Pdf.render");
    recordPerfName("roc__ptest_symbol_one", "Pdf.other");
    try std.testing.expectEqualStrings("Pdf.render", lookupPerfName("roc__ptest_symbol_one").?);
    try std.testing.expect(lookupPerfName("roc__ptest_symbol_two") == null);
}

/// Called only by owners of Boxy-only work, never by the specialized path.
pub fn traceBoxyWork(comptime activity: []const u8) void {
    if (isSet(.trace_boxy_work)) std.debug.print("boxy-work: " ++ activity ++ "\n", .{});
}
