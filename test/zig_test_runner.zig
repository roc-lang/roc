//! Roc's Zig test runner.
//!
//! The default Zig runner executes every test in a test binary serially. This
//! runner keeps Zig's server-mode protocol and leak detection, but lets the
//! build graph run multiple independent copies of the same test binary. Each
//! copy exposes only the tests whose compiler-provided index belongs to its
//! automatic numeric partition.

const builtin = @import("builtin");
const std = @import("std");

const Io = std.Io;
const testing = std.testing;

pub const std_options: std.Options = .{
    .logFn = log,
};

const runner_io: Io = Io.Threaded.global_single_threaded.io();

var log_err_count: usize = 0;

const RunnerConfig = struct {
    listen: bool = false,
    partition_index: usize = 0,
    partition_count: usize = 1,
    timing_name: ?[]const u8 = null,
    timing_generation_ns: ?i128 = null,
    // rwx-v1-json reporter (RWX Captain). When rwx_out_dir is set, write
    // <dir>/zig-tests-<suite_name>.json on completion. suite_name/suite_file tag the
    // results for Captain (per-binary output filename, test-id prefix, location.file).
    rwx_out_dir: ?[]const u8 = null,
    suite_name: ?[]const u8 = null,
    suite_file: ?[]const u8 = null,
    // Targeted retry: path to an rwx-v1-json file (Captain's {{ jsonFilePath }}) listing the
    // tests to re-run. When set, only tests whose id (<suite>::<name>) appears are selected.
    only_json: ?[]const u8 = null,
};

const ResultStatus = enum { pass, skip, fail };

const TestRecord = struct {
    test_index: u32,
    duration_ns: u64,
    status: ResultStatus,
    // Treated as failures by the rwx reporter (leak detection / logged errors are roc's
    // additional pass criteria beyond the test function's own return).
    leaked: bool = false,
    logged_errors: bool = false,
};

const timing_dir = ".zig-cache/roc-test-timings";
const max_timing_file_bytes = 16 * 1024 * 1024;
const max_only_json_bytes = 64 * 1024 * 1024;
const rwx_schema_url = "https://raw.githubusercontent.com/rwx-research/test-results-schema/main/v1.json";

pub fn main(init: std.process.Init.Minimal) void {
    @disableInstrumentation();

    if (builtin.cpu.arch.isSpirV()) return;
    if (builtin.fuzz) @panic("Roc partitioned test runner does not support fuzz tests");

    const allocator = std.heap.page_allocator;
    const args = init.args.toSlice(allocator) catch |err| {
        std.debug.panic("unable to parse command line args: {t}", .{err});
    };

    const config = parseArgs(args);
    if (config.partition_count == 0 or config.partition_index >= config.partition_count) {
        std.debug.panic(
            "invalid Roc test partition {d}/{d}",
            .{ config.partition_index, config.partition_count },
        );
    }

    if (config.listen) {
        mainServer(init, config) catch |err| {
            std.debug.panic("internal test runner failure: {t}", .{err});
        };
    } else {
        mainTerminal(init, config);
    }
}

fn parseArgs(args: []const []const u8) RunnerConfig {
    var config: RunnerConfig = .{};

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--listen=-")) {
            config.listen = true;
        } else if (std.mem.startsWith(u8, arg, "--seed=")) {
            testing.random_seed = std.fmt.parseUnsigned(u32, arg["--seed=".len..], 0) catch {
                @panic("unable to parse --seed command line argument");
            };
        } else if (std.mem.startsWith(u8, arg, "--cache-dir=")) {
            // Fuzz tests use this. Normal tests do not need it, but Zig's
            // build runner always passes it in server mode.
        } else if (std.mem.startsWith(u8, arg, "--roc-test-partition-index=")) {
            config.partition_index = std.fmt.parseUnsigned(
                usize,
                arg["--roc-test-partition-index=".len..],
                10,
            ) catch @panic("unable to parse --roc-test-partition-index");
        } else if (std.mem.startsWith(u8, arg, "--roc-test-partition-count=")) {
            config.partition_count = std.fmt.parseUnsigned(
                usize,
                arg["--roc-test-partition-count=".len..],
                10,
            ) catch @panic("unable to parse --roc-test-partition-count");
        } else if (std.mem.startsWith(u8, arg, "--roc-test-timing-name=")) {
            config.timing_name = arg["--roc-test-timing-name=".len..];
        } else if (std.mem.startsWith(u8, arg, "--roc-test-timing-generation-ns=")) {
            config.timing_generation_ns = std.fmt.parseInt(
                i128,
                arg["--roc-test-timing-generation-ns=".len..],
                10,
            ) catch @panic("unable to parse --roc-test-timing-generation-ns");
        } else if (std.mem.startsWith(u8, arg, "--roc-test-rwx-out=")) {
            config.rwx_out_dir = arg["--roc-test-rwx-out=".len..];
        } else if (std.mem.startsWith(u8, arg, "--roc-test-suite=")) {
            config.suite_name = arg["--roc-test-suite=".len..];
        } else if (std.mem.startsWith(u8, arg, "--roc-test-file=")) {
            config.suite_file = arg["--roc-test-file=".len..];
        } else if (std.mem.startsWith(u8, arg, "--roc-test-only-json=")) {
            config.only_json = arg["--roc-test-only-json=".len..];
        } else {
            std.debug.panic("unrecognized command line argument: {s}", .{arg});
        }
    }

    return config;
}

const TimingWeights = struct {
    allocator: std.mem.Allocator,
    map: std.StringHashMap(u64),

    fn init(allocator: std.mem.Allocator) TimingWeights {
        return .{
            .allocator = allocator,
            .map = std.StringHashMap(u64).init(allocator),
        };
    }

    fn deinit(self: *TimingWeights) void {
        var key_it = self.map.keyIterator();
        while (key_it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.map.deinit();
    }

    fn loadFile(self: *TimingWeights, path: []const u8) void {
        const contents = Io.Dir.cwd().readFileAlloc(
            runner_io,
            path,
            self.allocator,
            .limited(max_timing_file_bytes),
        ) catch return;
        defer self.allocator.free(contents);

        var lines = std.mem.splitScalar(u8, contents, '\n');
        while (lines.next()) |line| {
            if (line.len == 0) continue;
            const tab = std.mem.findScalar(u8, line, '\t') orelse continue;
            const duration_ns = std.fmt.parseUnsigned(u64, line[0..tab], 10) catch continue;
            const name = line[tab + 1 ..];
            if (name.len == 0) continue;

            if (self.map.getPtr(name)) |existing| {
                existing.* = duration_ns;
                continue;
            }

            const owned_name = self.allocator.dupe(u8, name) catch continue;
            self.map.put(owned_name, duration_ns) catch {
                self.allocator.free(owned_name);
                continue;
            };
        }
    }

    fn weight(self: *const TimingWeights, name: []const u8, default_weight: u64) u64 {
        return self.map.get(name) orelse default_weight;
    }
};

fn validateTimingName(name: []const u8) void {
    if (std.mem.findScalar(u8, name, '/') != null or
        std.mem.findScalar(u8, name, '\\') != null)
    {
        @panic("roc test timing name must not contain a path separator");
    }
}

fn timingPath(
    allocator: std.mem.Allocator,
    name: []const u8,
    partition_count: usize,
    generation_ns: i128,
    partition_index: usize,
) std.mem.Allocator.Error![]const u8 {
    validateTimingName(name);
    return std.fmt.allocPrint(
        allocator,
        "{s}/zig-{s}.{d}.{d}.{d}.tsv",
        .{ timing_dir, name, partition_count, generation_ns, partition_index },
    );
}

fn timingPrefix(allocator: std.mem.Allocator, name: []const u8, partition_count: usize) std.mem.Allocator.Error![]const u8 {
    validateTimingName(name);
    return std.fmt.allocPrint(allocator, "zig-{s}.{d}.", .{ name, partition_count });
}

fn timingGenerationPrefix(
    allocator: std.mem.Allocator,
    name: []const u8,
    partition_count: usize,
    generation_ns: i128,
) std.mem.Allocator.Error![]const u8 {
    validateTimingName(name);
    return std.fmt.allocPrint(allocator, "zig-{s}.{d}.{d}.", .{ name, partition_count, generation_ns });
}

fn loadTimingWeights(allocator: std.mem.Allocator, config: RunnerConfig) TimingWeights {
    var weights = TimingWeights.init(allocator);
    const name = config.timing_name orelse return weights;

    const prefix = timingPrefix(allocator, name, config.partition_count) catch return weights;
    defer allocator.free(prefix);

    const current_generation_prefix = if (config.timing_generation_ns) |generation_ns|
        timingGenerationPrefix(allocator, name, config.partition_count, generation_ns) catch null
    else
        null;
    defer if (current_generation_prefix) |value| allocator.free(value);

    var dir = Io.Dir.cwd().openDir(runner_io, timing_dir, .{ .iterate = true }) catch return weights;
    defer dir.close(runner_io);

    var iter = dir.iterate();
    while (iter.next(runner_io) catch null) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.startsWith(u8, entry.name, prefix)) continue;
        if (current_generation_prefix) |current| {
            if (std.mem.startsWith(u8, entry.name, current)) continue;
        }

        const path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ timing_dir, entry.name }) catch continue;
        defer allocator.free(path);
        weights.loadFile(path);
    }

    return weights;
}

/// rwx-v1-json id for a test: "<suite>::<zig test name>". Globally unique across module
/// binaries (the zig name alone is only unique within a binary). Caller owns the result.
fn testId(allocator: std.mem.Allocator, suite_name: ?[]const u8, test_name: []const u8) std.mem.Allocator.Error![]u8 {
    const suite = suite_name orelse "zig";
    return std.fmt.allocPrint(allocator, "{s}::{s}", .{ suite, test_name });
}

/// Load the set of test ids to re-run from Captain's {{ jsonFilePath }} rwx-v1-json file.
/// Returns null if no retry file is configured. Caller deinits + frees the arena via the
/// returned parsed handle.
const OnlyJsonIds = struct {
    parsed: std.json.Parsed(Doc),
    set: std.StringHashMap(void),

    const Doc = struct {
        tests: []const struct {
            id: ?[]const u8 = null,
        } = &.{},
    };

    fn deinit(self: *OnlyJsonIds) void {
        self.set.deinit();
        self.parsed.deinit();
    }
};

fn loadOnlyJsonIds(allocator: std.mem.Allocator, path: []const u8) ?OnlyJsonIds {
    const contents = Io.Dir.cwd().readFileAlloc(
        runner_io,
        path,
        allocator,
        .limited(max_only_json_bytes),
    ) catch return null;
    defer allocator.free(contents);

    const parsed = std.json.parseFromSlice(
        OnlyJsonIds.Doc,
        allocator,
        contents,
        // alloc_always: dupe strings into the parsed arena so the ids survive freeing
        // `contents` below (alloc_if_needed would leave them aliasing the freed buffer).
        .{ .ignore_unknown_fields = true, .allocate = .alloc_always },
    ) catch return null;

    var set = std.StringHashMap(void).init(allocator);
    for (parsed.value.tests) |t| {
        if (t.id) |id| set.put(id, {}) catch {};
    }
    return .{ .parsed = parsed, .set = set };
}

const WeightedTest = struct {
    index: u32,
    weight: u64,
};

fn defaultTestWeight(test_fn: anytype) u64 {
    _ = test_fn;
    return 1;
}

fn leastLoadedPartition(loads: []const u64) usize {
    var min_i: usize = 0;
    for (loads[1..], 1..) |load, i| {
        if (load < loads[min_i]) min_i = i;
    }
    return min_i;
}

fn buildSelectedIndices(allocator: std.mem.Allocator, config: RunnerConfig) std.mem.Allocator.Error![]u32 {
    var weights = loadTimingWeights(allocator, config);
    defer weights.deinit();

    const items = try allocator.alloc(WeightedTest, builtin.test_functions.len);
    defer allocator.free(items);
    for (builtin.test_functions, 0..) |test_fn, test_i| {
        items[test_i] = .{
            .index = @intCast(test_i),
            .weight = weights.weight(test_fn.name, defaultTestWeight(test_fn)),
        };
    }

    std.mem.sort(WeightedTest, items, {}, struct {
        fn lessThan(_: void, a: WeightedTest, b: WeightedTest) bool {
            if (a.weight != b.weight) return a.weight > b.weight;
            return a.index < b.index;
        }
    }.lessThan);

    const loads = try allocator.alloc(u64, config.partition_count);
    defer allocator.free(loads);
    @memset(loads, 0);

    var selected = std.ArrayList(u32).empty;
    errdefer selected.deinit(allocator);

    for (items) |item| {
        const owner = leastLoadedPartition(loads);
        if (owner == config.partition_index) {
            try selected.append(allocator, item.index);
        }
        loads[owner] +|= item.weight;
    }

    std.mem.sort(u32, selected.items, {}, struct {
        fn lessThan(_: void, a: u32, b: u32) bool {
            return a < b;
        }
    }.lessThan);

    // Targeted retry: keep only the tests Captain asked us to re-run (by id). Each module
    // binary runs only the failing tests it actually owns; binaries with no matches run none.
    if (config.only_json) |only_path| {
        if (loadOnlyJsonIds(allocator, only_path)) |loaded| {
            var ids = loaded;
            defer ids.deinit();

            var kept = std.ArrayList(u32).empty;
            errdefer kept.deinit(allocator);
            for (selected.items) |test_i| {
                const id = try testId(allocator, config.suite_name, builtin.test_functions[@intCast(test_i)].name);
                defer allocator.free(id);
                if (ids.set.contains(id)) try kept.append(allocator, test_i);
            }
            selected.deinit(allocator);
            return kept.toOwnedSlice(allocator);
        }
    }

    return selected.toOwnedSlice(allocator);
}

fn nowNs() u64 {
    const ns = Io.Timestamp.now(runner_io, .awake).nanoseconds;
    return @intCast(@max(0, ns));
}

fn writeTimingRecords(
    allocator: std.mem.Allocator,
    config: RunnerConfig,
    records: []const TestRecord,
) void {
    const name = config.timing_name orelse return;
    const generation_ns = config.timing_generation_ns orelse return;

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);

    for (records) |record| {
        const test_index: usize = @intCast(record.test_index);
        if (test_index >= builtin.test_functions.len) continue;
        const test_fn = builtin.test_functions[test_index];
        buf.print(allocator, "{d}\t{s}\n", .{ record.duration_ns, test_fn.name }) catch return;
    }

    Io.Dir.cwd().createDirPath(runner_io, timing_dir) catch return;

    const path = timingPath(allocator, name, config.partition_count, generation_ns, config.partition_index) catch return;
    defer allocator.free(path);
    Io.Dir.cwd().writeFile(runner_io, .{
        .sub_path = path,
        .data = buf.items,
    }) catch {};
}

// rwx-v1-json structs (https://github.com/rwx-research/test-results-schema/blob/main/v1.json).
// Zig has no native Captain framework, so we report as the generic "other" framework, which
// Captain ingests directly (rwx-v1-json is its own canonical schema — no custom parser needed).
const RwxStatus = struct { kind: []const u8 };
const RwxLocation = struct { file: []const u8 };
const RwxAttempt = struct { durationInNanoseconds: u64, status: RwxStatus };
const RwxTest = struct {
    id: []const u8,
    name: []const u8,
    location: RwxLocation,
    attempt: RwxAttempt,
};
const RwxFramework = struct {
    language: []const u8 = "other",
    kind: []const u8 = "other",
    providedLanguage: []const u8 = "Zig",
    providedKind: []const u8 = "roc",
};
const RwxSummary = struct {
    status: RwxStatus,
    tests: usize,
    otherErrors: usize = 0,
    retries: usize = 0,
    canceled: usize = 0,
    failed: usize,
    pended: usize = 0,
    quarantined: usize = 0,
    skipped: usize,
    successful: usize,
    timedOut: usize = 0,
    todo: usize = 0,
};
const RwxDoc = struct {
    @"$schema": []const u8 = rwx_schema_url,
    framework: RwxFramework = .{},
    summary: RwxSummary,
    tests: []const RwxTest,
};

/// Write this binary's tests as <rwx_out_dir>/zig-tests-<suite>.json. No-op when the reporter
/// is not enabled (-Droc-test-rwx-out unset). Best-effort: never fails the test run.
fn writeRwxResults(
    allocator: std.mem.Allocator,
    config: RunnerConfig,
    records: []const TestRecord,
) void {
    const out_dir = config.rwx_out_dir orelse return;
    const suite = config.suite_name orelse "zig";
    const file = config.suite_file orelse suite;

    var arena_inst = std.heap.ArenaAllocator.init(allocator);
    defer arena_inst.deinit();
    const arena = arena_inst.allocator();

    var tests = std.ArrayList(RwxTest).empty;
    var n_pass: usize = 0;
    var n_fail: usize = 0;
    var n_skip: usize = 0;
    for (records) |rec| {
        const ti: usize = @intCast(rec.test_index);
        if (ti >= builtin.test_functions.len) continue;
        const name = builtin.test_functions[ti].name;
        // roc treats a leak or a logged error as a failure, beyond the test fn's own result.
        const failed = rec.status == .fail or rec.leaked or rec.logged_errors;
        const kind: []const u8 = if (rec.status == .skip) "skipped" else if (failed) "failed" else "successful";
        if (rec.status == .skip) {
            n_skip += 1;
        } else if (failed) {
            n_fail += 1;
        } else {
            n_pass += 1;
        }
        tests.append(arena, .{
            .id = testId(arena, suite, name) catch continue,
            .name = name,
            .location = .{ .file = file },
            .attempt = .{ .durationInNanoseconds = rec.duration_ns, .status = .{ .kind = kind } },
        }) catch continue;
    }

    const doc = RwxDoc{
        .summary = .{
            .status = .{ .kind = if (n_fail > 0) "failed" else "successful" },
            .tests = tests.items.len,
            .failed = n_fail,
            .skipped = n_skip,
            .successful = n_pass,
        },
        .tests = tests.items,
    };

    const json = std.json.Stringify.valueAlloc(arena, doc, .{ .whitespace = .indent_2 }) catch return;

    Io.Dir.cwd().createDirPath(runner_io, out_dir) catch return;
    const path = std.fmt.allocPrint(arena, "{s}/zig-tests-{s}.json", .{ out_dir, suite }) catch return;
    Io.Dir.cwd().writeFile(runner_io, .{ .sub_path = path, .data = json }) catch {};
}

fn mainServer(init: std.process.Init.Minimal, config: RunnerConfig) anyerror!void {
    @disableInstrumentation();

    const allocator = std.heap.page_allocator;
    const stdin_buffer = try allocator.alloc(u8, 4096);
    defer allocator.free(stdin_buffer);
    const stdout_buffer = try allocator.alloc(u8, 4096);
    defer allocator.free(stdout_buffer);

    var stdin_reader = Io.File.Reader.initStreaming(.stdin(), runner_io, stdin_buffer);
    var stdout_writer = Io.File.Writer.initStreaming(.stdout(), runner_io, stdout_buffer);
    var server = try std.zig.Server.init(.{
        .in = &stdin_reader.interface,
        .out = &stdout_writer.interface,
        .zig_version = builtin.zig_version_string,
    });

    var selected_indices: []u32 = &.{};
    defer if (selected_indices.len != 0) allocator.free(selected_indices);
    var test_records = std.ArrayList(TestRecord).empty;
    defer test_records.deinit(allocator);

    while (true) {
        const hdr = try server.receiveMessage();
        switch (hdr.tag) {
            .exit => {
                writeTimingRecords(allocator, config, test_records.items);
                writeRwxResults(allocator, config, test_records.items);
                return std.process.exit(0);
            },

            .query_test_metadata => {
                testing.allocator_instance = .{};
                defer if (testing.allocator_instance.deinit() == .leak) {
                    @panic("internal test runner memory leak");
                };

                if (selected_indices.len != 0) {
                    allocator.free(selected_indices);
                    selected_indices = &.{};
                }

                selected_indices = try buildSelectedIndices(allocator, config);
                const selected_count = selected_indices.len;

                var string_bytes: std.ArrayList(u8) = .empty;
                defer string_bytes.deinit(testing.allocator);
                try string_bytes.append(testing.allocator, 0);

                const names = try testing.allocator.alloc(u32, selected_count);
                defer testing.allocator.free(names);
                const expected_panic_msgs = try testing.allocator.alloc(u32, selected_count);
                defer testing.allocator.free(expected_panic_msgs);

                for (selected_indices, 0..) |test_i, selected_i| {
                    const test_fn = builtin.test_functions[@as(usize, @intCast(test_i))];
                    names[selected_i] = @intCast(string_bytes.items.len);
                    expected_panic_msgs[selected_i] = 0;

                    try string_bytes.ensureUnusedCapacity(testing.allocator, test_fn.name.len + 1);
                    string_bytes.appendSliceAssumeCapacity(test_fn.name);
                    string_bytes.appendAssumeCapacity(0);
                }

                try server.serveTestMetadata(.{
                    .names = names,
                    .expected_panic_msgs = expected_panic_msgs,
                    .string_bytes = string_bytes.items,
                });
            },

            .run_test => {
                testing.environ = init.environ;
                testing.allocator_instance = .{};
                testing.io_instance = .init(testing.allocator, .{
                    .argv0 = .init(init.args),
                    .environ = init.environ,
                });
                testing.log_level = .warn;
                log_err_count = 0;

                const metadata_index = try server.receiveBody_u32();

                // Normally `query_test_metadata` runs first and populates `selected_indices`.
                // But when a test hard-aborts (e.g. a `std.debug.panic` in a Debug build), Zig's
                // build runner re-spawns this binary to continue the remaining tests, replaying
                // `run_test` against the metadata it cached from the first run *without*
                // re-querying (std.Build.Step.Run only sends `query_test_metadata` when its
                // metadata is null). The stock runner is immune because it indexes
                // `builtin.test_functions` directly; our partition indirection is not. Rebuild
                // lazily so the cached index still maps. `buildSelectedIndices` is deterministic
                // for a given config, so the index stays valid; this keeps the crash isolated to
                // the offending test (so the rest of the suite still reports) instead of masking
                // it with a runner-internal index error on every re-spawn.
                if (selected_indices.len == 0) {
                    selected_indices = try buildSelectedIndices(allocator, config);
                }
                if (metadata_index >= selected_indices.len) {
                    std.debug.panic(
                        "run_test index {d} out of range for {d} selected test(s)",
                        .{ metadata_index, selected_indices.len },
                    );
                }
                const test_fn = builtin.test_functions[selected_indices[metadata_index]];

                try server.serveStringMessage(.test_started, &.{});

                const TestResults = std.zig.Server.Message.TestResults;
                const started_ns = nowNs();
                const status: TestResults.Status = if (test_fn.func()) |_| status: {
                    break :status .pass;
                } else |err| switch (err) {
                    error.SkipZigTest => .skip,
                    else => status: {
                        if (@errorReturnTrace()) |trace| {
                            std.debug.dumpErrorReturnTrace(trace);
                        }
                        break :status .fail;
                    },
                };
                const duration_ns = nowNs() - started_ns;

                testing.io_instance.deinit();
                const leak_count = testing.allocator_instance.detectLeaks();
                testing.allocator_instance.deinitWithoutLeakChecks();

                test_records.append(allocator, .{
                    .test_index = selected_indices[metadata_index],
                    .duration_ns = duration_ns,
                    .status = switch (status) {
                        .pass => .pass,
                        .skip => .skip,
                        else => .fail,
                    },
                    .leaked = leak_count > 0,
                    .logged_errors = log_err_count > 0,
                }) catch {};

                try server.serveTestResults(.{
                    .index = metadata_index,
                    .flags = .{
                        .status = status,
                        .fuzz = false,
                        .log_err_count = std.math.lossyCast(
                            @FieldType(TestResults.Flags, "log_err_count"),
                            log_err_count,
                        ),
                        .leak_count = std.math.lossyCast(
                            @FieldType(TestResults.Flags, "leak_count"),
                            leak_count,
                        ),
                    },
                });
            },

            else => {
                std.debug.print("unsupported message: {x}\n", .{@intFromEnum(hdr.tag)});
                std.process.exit(1);
            },
        }
    }
}

fn mainTerminal(init: std.process.Init.Minimal, config: RunnerConfig) void {
    @disableInstrumentation();

    const test_fn_list = builtin.test_functions;
    const allocator = std.heap.page_allocator;
    const selected_indices = buildSelectedIndices(allocator, config) catch {
        @panic("unable to build selected test indices");
    };
    defer allocator.free(selected_indices);
    var test_records = std.ArrayList(TestRecord).empty;
    defer test_records.deinit(allocator);

    const selected_count = selected_indices.len;
    var ok_count: usize = 0;
    var skip_count: usize = 0;
    var fail_count: usize = 0;

    const root_node = std.Progress.start(runner_io, .{
        .root_name = "Test",
        .estimated_total_items = selected_count,
    });
    const have_tty = Io.File.stderr().isTty(runner_io) catch unreachable;

    var leaks: usize = 0;
    var selected_i: usize = 0;
    for (selected_indices) |test_i| {
        const test_fn = test_fn_list[@as(usize, @intCast(test_i))];
        selected_i += 1;

        testing.allocator_instance = .{};
        testing.io_instance = .init(testing.allocator, .{
            .argv0 = .init(init.args),
            .environ = init.environ,
        });
        testing.log_level = .warn;
        testing.environ = init.environ;
        log_err_count = 0;

        const test_node = root_node.start(test_fn.name, 0);
        if (!have_tty) {
            std.debug.print("{d}/{d} {s}...", .{ selected_i, selected_count, test_fn.name });
        }
        const started_ns = nowNs();
        // Capture the outcome and dump the failure trace here (before any further fallible
        // calls clobber the error-return trace), then tear down to learn the leak result.
        var status: ResultStatus = .pass;
        var caught_err: ?anyerror = null;
        if (test_fn.func()) |_| {} else |err| {
            caught_err = err;
            switch (err) {
                error.SkipZigTest => status = .skip,
                else => {
                    status = .fail;
                    if (@errorReturnTrace()) |trace| std.debug.dumpErrorReturnTrace(trace);
                },
            }
        }
        const duration_ns = nowNs() - started_ns;
        const logged_errors = log_err_count > 0;

        testing.io_instance.deinit();
        const leaked = testing.allocator_instance.deinit() == .leak;
        if (leaked) leaks += 1;

        switch (status) {
            .pass => {
                ok_count += 1;
                if (!have_tty) std.debug.print("OK\n", .{});
            },
            .skip => {
                skip_count += 1;
                if (have_tty) {
                    std.debug.print("{d}/{d} {s}...SKIP\n", .{ selected_i, selected_count, test_fn.name });
                } else {
                    std.debug.print("SKIP\n", .{});
                }
            },
            .fail => {
                fail_count += 1;
                if (have_tty) {
                    std.debug.print("{d}/{d} {s}...FAIL ({t})\n", .{
                        selected_i, selected_count, test_fn.name, caught_err.?,
                    });
                } else {
                    std.debug.print("FAIL ({t})\n", .{caught_err.?});
                }
            },
        }
        test_node.end();

        test_records.append(allocator, .{
            .test_index = test_i,
            .duration_ns = duration_ns,
            .status = status,
            .leaked = leaked,
            .logged_errors = logged_errors,
        }) catch {};
    }
    root_node.end();
    writeTimingRecords(allocator, config, test_records.items);
    writeRwxResults(allocator, config, test_records.items);

    if (ok_count == selected_count) {
        std.debug.print("All {d} tests passed.\n", .{ok_count});
    } else {
        std.debug.print("{d} passed; {d} skipped; {d} failed.\n", .{ ok_count, skip_count, fail_count });
    }
    if (log_err_count != 0) {
        std.debug.print("{d} errors were logged.\n", .{log_err_count});
    }
    if (leaks != 0) {
        std.debug.print("{d} tests leaked memory.\n", .{leaks});
    }
    if (leaks != 0 or log_err_count != 0 or fail_count != 0) {
        std.process.exit(1);
    }
}

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    @disableInstrumentation();
    if (@intFromEnum(message_level) <= @intFromEnum(std.log.Level.err)) {
        log_err_count +|= 1;
    }
    if (@intFromEnum(message_level) <= @intFromEnum(testing.log_level)) {
        std.debug.print(
            "[" ++ @tagName(scope) ++ "] (" ++ @tagName(message_level) ++ "): " ++ format ++ "\n",
            args,
        );
    }
}
