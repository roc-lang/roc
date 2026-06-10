const std = @import("std");
const builtin = @import("builtin");
const modules = @import("src/build/modules.zig");
const glibc_stub_build = @import("src/build/glibc_stub.zig");
const ci_steps = @import("src/build/ci_steps.zig");
const roc_target = @import("src/target/mod.zig");
const Dependency = std.Build.Dependency;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = std.Build.ResolvedTarget;
const Step = std.Build.Step;

// Cross-compile target definitions

/// Cross-compile target specification
const CrossTarget = struct {
    name: []const u8,
    query: std.Target.Query,
};

/// Musl-only cross-compile targets (static linking)
const musl_cross_targets = [_]CrossTarget{
    .{ .name = "x64musl", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl } },
    .{ .name = "arm64musl", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl } },
};

/// Glibc cross-compile targets (dynamic linking)
const glibc_cross_targets = [_]CrossTarget{
    .{ .name = "x64glibc", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu } },
    .{ .name = "arm64glibc", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu } },
};

/// Windows cross-compile targets
const windows_cross_targets = [_]CrossTarget{
    .{ .name = "x64win", .query = .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .msvc } },
    .{ .name = "arm64win", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .msvc } },
};

/// All Linux cross-compile targets (musl + glibc)
const linux_cross_targets = musl_cross_targets ++ glibc_cross_targets;

/// Test platform directories that need host libraries built
const all_test_platform_dirs = [_][]const u8{ "str", "int", "fx", "fx-open", "dylib" };

fn mustUseLlvm(target: ResolvedTarget) bool {
    return target.result.os.tag == .macos and target.result.cpu.arch == .x86_64;
}

fn testHostNeedsCompilerRt(target: ResolvedTarget) bool {
    return mustUseLlvm(target) or
        (target.result.os.tag == .windows and target.result.cpu.arch == .aarch64);
}

fn configureBackend(step: *Step.Compile, target: ResolvedTarget) void {
    if (mustUseLlvm(target)) {
        step.use_llvm = true;
    }
}

const TestHostOptions = struct {
    uses_stack_handler: bool = false,
};

fn testPlatformUsesStackHandler(platform_dir: []const u8) bool {
    return std.mem.eql(u8, platform_dir, "fx");
}

fn testHostNeedsLibc(options: TestHostOptions, target: ResolvedTarget) bool {
    if (!options.uses_stack_handler) return false;

    return switch (target.result.os.tag) {
        .linux,
        .macos,
        .ios,
        .tvos,
        .watchos,
        .visionos,
        .freebsd,
        .dragonfly,
        .netbsd,
        .openbsd,
        => true,
        else => false,
    };
}

fn isNativeishOrMusl(target: ResolvedTarget) bool {
    return target.result.cpu.arch == builtin.target.cpu.arch and
        target.result.os.tag == builtin.target.os.tag and
        (target.query.isNativeAbi() or target.result.abi.isMusl());
}

/// Returns the optimal target query for release builds on the current host.
/// - Linux: Uses musl for fully static binaries
/// - x86_64: Uses x86_64_v3 for modern CPU features (AVX2, BMI2, etc.)
fn getReleaseTargetQuery() std.Target.Query {
    var query: std.Target.Query = .{};

    // Use musl on Linux for static linking
    if (builtin.target.os.tag == .linux) {
        query.abi = .musl;
    }

    // Use x86_64_v3 CPU model for x86_64 (enables AVX2, BMI2, etc.)
    if (builtin.target.cpu.arch == .x86_64) {
        query.cpu_model = .{ .explicit = &std.Target.x86.cpu.x86_64_v3 };
    }

    return query;
}

const TestsSummaryStep = struct {
    step: Step,
    has_filters: bool,
    test_filters: []const []const u8,
    forced_passes: u64,
    run_prerequisite: ?*Step,
    serialize_runs: bool,
    last_run: ?*Step,

    fn create(
        b: *std.Build,
        test_filters: []const []const u8,
        forced_passes: usize,
    ) *TestsSummaryStep {
        const self = b.allocator.create(TestsSummaryStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "tests_summary",
                .owner = b,
                .makeFn = make,
            }),
            .has_filters = test_filters.len > 0,
            .test_filters = test_filters,
            .forced_passes = @intCast(forced_passes),
            .run_prerequisite = null,
            .serialize_runs = false,
            .last_run = null,
        };
        return self;
    }

    fn setRunSerialization(self: *TestsSummaryStep) void {
        self.serialize_runs = true;
    }

    fn addRun(self: *TestsSummaryStep, run_step: *Step) void {
        if (self.run_prerequisite) |prerequisite| {
            run_step.dependOn(prerequisite);
        }
        if (self.serialize_runs) {
            if (self.last_run) |last_run| {
                run_step.dependOn(last_run);
            }
            self.last_run = run_step;
        }
        self.step.dependOn(run_step);
    }

    /// Returns the position of the last '.' within the common prefix of a and b.
    /// Returns 0 if there is no shared dot-delimited prefix.
    fn commonDotPrefix(a: []const u8, b: []const u8) usize {
        const min_len = @min(a.len, b.len);
        var last_dot: usize = 0;
        for (0..min_len) |i| {
            if (a[i] != b[i]) break;
            if (a[i] == '.') last_dot = i;
        }
        return last_dot;
    }

    /// Returns true if the test name contains any of the user's filter strings.
    fn matchesUserFilter(test_filters: []const []const u8, name: []const u8) bool {
        for (test_filters) |filter| {
            if (std.mem.find(u8, name, filter) != null) return true;
        }
        return false;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;

        const self: *TestsSummaryStep = @fieldParentPtr("step", step);

        var passed: u64 = 0;

        for (step.dependencies.items) |dependency| {
            const module_pass_count = dependency.test_results.passCount();
            passed += @intCast(module_pass_count);
        }

        var effective_passed = passed;
        if (self.has_filters and self.forced_passes != 0) {
            const subtract = @min(effective_passed, self.forced_passes);
            effective_passed -= subtract;
        }

        // When filters are active, print the names of all tests that matched.
        // Consecutive tests sharing a common dot-delimited prefix are shown
        // in a compact form to avoid visual repetition.
        if (self.has_filters and effective_passed > 0) {
            const max_indent = 256;
            const spaces = [_]u8{' '} ** max_indent;
            var prev_module: []const u8 = "";
            var prev_name: []const u8 = "";

            for (step.dependencies.items) |dependency| {
                if (dependency.id != .run) continue;
                const run: *std.Build.Step.Run = @fieldParentPtr("step", dependency);
                const tm = run.cached_test_metadata orelse continue;
                const module_name = if (run.producer) |p| p.name else "unknown";
                for (tm.names) |name_offset| {
                    const name = std.mem.sliceTo(tm.string_bytes[name_offset..], 0);
                    if (name.len == 0) continue;
                    if (!matchesUserFilter(self.test_filters, name)) continue;

                    const shared_dot = if (std.mem.eql(u8, module_name, prev_module))
                        commonDotPrefix(name, prev_name)
                    else
                        0;

                    if (shared_dot > 0) {
                        const indent_len = @min(2 + module_name.len + 2 + shared_dot, max_indent);
                        std.debug.print("{s}{s}\n", .{ spaces[0..indent_len], name[shared_dot..] });
                    } else {
                        std.debug.print("  {s}: {s}\n", .{ module_name, name });
                    }
                    prev_module = module_name;
                    prev_name = name;
                }
            }
        }

        if (effective_passed == 0) {
            std.debug.print("No tests ran (all tests filtered out).\n", .{});
        } else {
            std.debug.print("All {d} tests passed.\n", .{effective_passed});
        }
    }
};

/// Build step that checks for forbidden patterns in the type checker code.
///
/// During type checking, we NEVER do string or byte comparisons because:
/// 1. They take linear time, which can cause performance issues
/// 2. They are brittle to changes that type-checking should not be sensitive to
///
/// Instead, we always compare indices - either into node stores or to interned string indices.
/// This step enforces that rule by failing the build if `std.mem.` is found in src/canonicalize/, src/check/, src/layout/, or src/eval/.
const CheckTypeCheckerPatternsStep = struct {
    step: Step,

    fn create(b: *std.Build) *CheckTypeCheckerPatternsStep {
        const self = b.allocator.create(CheckTypeCheckerPatternsStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "check-type-checker-patterns",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, _: Step.MakeOptions) !void {
        const b = step.owner;
        const allocator = b.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Recursively scan src/canonicalize/, src/check/, src/layout/, and src/eval/ for .zig files
        // TODO: uncomment "src/canonicalize" once its std.mem violations are fixed
        const dirs_to_scan = [_][]const u8{ "src/check", "src/layout", "src/eval" };
        for (dirs_to_scan) |dir_path| {
            const io = step.owner.graph.io;
            var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| {
                return step.fail("Failed to open {s} directory: {}", .{ dir_path, err });
            };
            defer dir.close(io);

            try scanDirectory(allocator, io, dir, dir_path, &violations);
        }

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("FORBIDDEN PATTERN DETECTED\n", .{});
            std.debug.print("=" ** 80 ++ "\n\n", .{});

            std.debug.print(
                \\Code in src/canonicalize/, src/check/, src/layout/, and src/eval/ must NOT do raw string comparison or manipulation.
                \\
                \\WHY THIS RULE EXISTS:
                \\  We NEVER do string or byte comparisons because:
                \\
                \\  1. PERFORMANCE: String comparisons take O(n) time where n is the string
                \\     length. These code paths can involve many comparisons, so this adds up.
                \\
                \\  2. BRITTLENESS: String comparisons make the code sensitive to changes it
                \\     shouldn't care about (e.g., how identifiers are rendered, whitespace,
                \\     formatting). This leads to subtle bugs.
                \\
                \\WHAT TO DO INSTEAD:
                \\  Always compare indices rather than strings:
                \\
                \\  - For identifiers: Compare Ident.Idx values (interned string indices)
                \\  - For types: Compare type variable indices or node store indices
                \\  - For expressions: Compare Expr.Idx values from the node store
                \\
                \\  Example - WRONG:
                \\    if (std.mem.eql(u8, ident_name, "is_eq")) {{ ... }}
                \\
                \\  Example - RIGHT:
                \\    if (ident_idx == module_env.idents.is_eq) {{ ... }}
                \\
                \\VIOLATIONS FOUND:
                \\
            , .{});

            for (violations.items) |violation| {
                std.debug.print("  {s}:{d}: {s}\n", .{
                    violation.file_path,
                    violation.line_number,
                    violation.line_content,
                });
            }

            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});

            return step.fail(
                "Found {d} forbidden patterns (raw string comparison or manipulation) in src/canonicalize/, src/check/, src/layout/, or src/eval/. " ++
                    "See above for details on why this is forbidden and what to do instead.",
                .{violations.items.len},
            );
        }
    }

    const Violation = struct {
        file_path: []const u8,
        line_number: usize,
        line_content: []const u8,
    };

    const ExcludedRange = struct { file: []const u8, start: usize, end: usize };
    const excluded_ranges = [_]ExcludedRange{
        // Cross-module name matching in Check.zig requires string comparison (lines 5530-5547)
        // This is necessary because origin_module is an ident from the type's defining module,
        // while module_name is from the importing module's ident store - no way to compare without strings
        .{ .file = "Check.zig", .start = 5530, .end = 5547 },
        // Cross-module nominal type matching in store.zig requires string comparison
        // because ident indices are module-local — same nominal from different modules
        // has different Ident.Idx values, so we must compare the underlying strings
        .{ .file = "store.zig", .start = 340, .end = 355 },
        // Cross-module ident matching in cir_to_lir.zig requires string comparison
        // because platform and app modules have separate ident stores — the same alias
        // name has different Ident.Idx values across modules, so we must compare via text.
        .{ .file = "cir_to_lir.zig", .start = 110, .end = 115 },
    };

    fn isInExcludedRange(file_path: []const u8, line_number: usize) bool {
        for (excluded_ranges) |range| {
            if (std.mem.endsWith(u8, file_path, range.file)) {
                if (line_number >= range.start and line_number <= range.end) {
                    return true;
                }
            }
        }
        return false;
    }

    fn scanDirectory(
        allocator: std.mem.Allocator,
        io: std.Io,
        dir: std.Io.Dir,
        path_prefix: []const u8,
        violations: *std.ArrayList(Violation),
    ) !void {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next(io)) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

            // Skip test files - they may legitimately need string comparison for assertions
            if (std.mem.endsWith(u8, entry.path, "_test.zig")) continue;
            if (std.mem.find(u8, entry.path, "test/") != null) continue;
            if (std.mem.startsWith(u8, entry.path, "test")) continue;
            if (std.mem.endsWith(u8, entry.path, "test_runner.zig")) continue;

            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

            const content = dir.readFileAlloc(io, entry.path, allocator, .limited(10 * 1024 * 1024)) catch continue;
            defer allocator.free(content);

            var line_number: usize = 1;
            var line_start: usize = 0;

            for (content, 0..) |char, i| {
                if (char == '\n') {
                    const line = content[line_start..i];

                    const trimmed = std.mem.trim(u8, line, " \t");
                    // Skip comments
                    if (std.mem.startsWith(u8, trimmed, "//")) {
                        line_number += 1;
                        line_start = i + 1;
                        continue;
                    }

                    // Check for std.mem. usage (but allow safe patterns)
                    if (std.mem.find(u8, line, "std.mem.")) |idx| {
                        const after_match = line[idx + 8 ..];

                        // Allow these safe patterns that don't involve string/byte comparison:
                        // - std.mem.Allocator: a type, not a comparison
                        // - std.mem.Alignment: a type, not a comparison
                        // - std.mem.sort: sorting by custom comparator, not string comparison
                        // - std.mem.asBytes: type punning, not string comparison
                        // - std.mem.reverse: reversing arrays, not string comparison
                        // - std.mem.alignForward: memory alignment arithmetic, not string comparison
                        // - std.mem.order: sort ordering (used by sort comparators), not string comparison
                        // - std.mem.copyForwards: byte copying, not string comparison
                        const is_allowed =
                            std.mem.startsWith(u8, after_match, "Allocator") or
                            std.mem.startsWith(u8, after_match, "Alignment") or
                            std.mem.startsWith(u8, after_match, "sort") or
                            std.mem.startsWith(u8, after_match, "asBytes") or
                            std.mem.startsWith(u8, after_match, "reverse") or
                            std.mem.startsWith(u8, after_match, "alignForward") or
                            std.mem.startsWith(u8, after_match, "order") or
                            std.mem.startsWith(u8, after_match, "copyForwards");

                        if (!is_allowed and !isInExcludedRange(full_path, line_number)) {
                            try violations.append(allocator, .{
                                .file_path = full_path,
                                .line_number = line_number,
                                .line_content = try allocator.dupe(u8, trimmed),
                            });
                        }
                    }

                    // Check for findByString usage - should use Ident.Idx comparison instead
                    if (std.mem.find(u8, line, "findByString") != null and !isInExcludedRange(full_path, line_number)) {
                        try violations.append(allocator, .{
                            .file_path = full_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                        });
                    }

                    // Check for findIdent usage - should use pre-stored Ident.Idx instead
                    if (std.mem.find(u8, line, "findIdent") != null and !isInExcludedRange(full_path, line_number)) {
                        try violations.append(allocator, .{
                            .file_path = full_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                        });
                    }

                    // Check for getMethodIdent usage - should use pre-stored Ident.Idx instead
                    if (std.mem.find(u8, line, "getMethodIdent") != null and !isInExcludedRange(full_path, line_number)) {
                        try violations.append(allocator, .{
                            .file_path = full_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                        });
                    }

                    line_number += 1;
                    line_start = i + 1;
                }
            }
        }
    }
};

/// Header marker present in files vendored from the Zig compiler. Such files
/// are exempt from Roc's architecture-style checks (the @enumFromInt(0) and
/// unused-suppression bans below): their idioms — e.g. zero-valued enum
/// constants like `AddrSpace = @enumFromInt(0)` and `_ =` suppressions in
/// upstream TODO stubs — are correct at the source and rewriting them would
/// only diverge from upstream. This mirrors how ci/tidy.zig skips crates/.
const vendored_zig_marker = "Adapted from the Zig compiler";

/// Build step that checks for @enumFromInt(0) usage in all .zig files.
///
/// We forbid @enumFromInt(0) because it hides bugs and makes them harder to debug.
/// If we need a placeholder value that we believe will never be read, we should
/// use `undefined` instead - that way our intent is clear, and it can fail in a
/// more obvious way if our assumption is incorrect.
const CheckEnumFromIntZeroStep = struct {
    step: Step,

    fn create(b: *std.Build) *CheckEnumFromIntZeroStep {
        const self = b.allocator.create(CheckEnumFromIntZeroStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "check-enum-from-int-zero",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;
        const b = step.owner;
        const allocator = b.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Recursively scan src/ for .zig files
        const io = step.owner.graph.io;
        var dir = std.Io.Dir.cwd().openDir(io, "src", .{ .iterate = true }) catch |err| {
            return step.fail("Failed to open src directory: {}", .{err});
        };
        defer dir.close(io);

        try scanDirectoryForEnumFromIntZero(allocator, io, dir, "src", &violations);

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("FORBIDDEN PATTERN: @enumFromInt(0)\n", .{});
            std.debug.print("=" ** 80 ++ "\n\n", .{});

            std.debug.print(
                \\Using @enumFromInt(0) is forbidden in this codebase.
                \\
                \\WHY THIS RULE EXISTS:
                \\  @enumFromInt(0) hides bugs and makes them harder to debug. It creates
                \\  a "valid-looking" value that can silently propagate through the code
                \\  when something goes wrong.
                \\
                \\WHAT TO DO INSTEAD:
                \\  If you need a placeholder value that you believe will never be read,
                \\  use `undefined` instead. This makes your intent clear, and if your
                \\  assumption is wrong and the value IS read, it will fail more obviously.
                \\
                \\  When using `undefined`, add a comment explaining why it's correct there
                \\  (e.g., where it will be overwritten before being read).
                \\
                \\  Example - WRONG:
                \\    .anno = @enumFromInt(0), // placeholder - will be replaced
                \\
                \\  Example - RIGHT:
                \\    .anno = undefined, // overwritten in Phase 1.7 before use
                \\
                \\VIOLATIONS FOUND:
                \\
            , .{});

            for (violations.items) |violation| {
                std.debug.print("  {s}:{d}: {s}\n", .{
                    violation.file_path,
                    violation.line_number,
                    violation.line_content,
                });
            }

            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});

            return step.fail(
                "Found {d} uses of @enumFromInt(0). Using placeholder values like this has consistently led to bugs in this code base. " ++
                    "Do not use @enumFromInt(0) and also do not uncritically replace it with another placeholder like .first or something like that. " ++
                    "If you want it to be uninitialized and are very confident it will be overwritten before it is ever read, then use `undefined`. " ++
                    "Otherwise, take a step back and rethink how this code works; there should be a way to implement this in a way that does not use hardcoded placeholder indices like 0! " ++
                    "See above for details.",
                .{violations.items.len},
            );
        }
    }

    const Violation = struct {
        file_path: []const u8,
        line_number: usize,
        line_content: []const u8,
    };

    fn scanDirectoryForEnumFromIntZero(
        allocator: std.mem.Allocator,
        io: std.Io,
        dir: std.Io.Dir,
        path_prefix: []const u8,
        violations: *std.ArrayList(Violation),
    ) !void {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next(io)) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

            const content = dir.readFileAlloc(io, entry.path, allocator, .limited(10 * 1024 * 1024)) catch continue;
            defer allocator.free(content);

            // Vendored Zig-compiler files use upstream idioms this check would
            // flag (e.g. zero-valued enum constants like `AddrSpace = @enumFromInt(0)`);
            // exempt them, mirroring how ci/tidy.zig skips crates/.
            if (std.mem.find(u8, content, vendored_zig_marker) != null) continue;

            var line_number: usize = 1;
            var line_start: usize = 0;

            for (content, 0..) |char, i| {
                if (char == '\n') {
                    const line = content[line_start..i];

                    const trimmed = std.mem.trim(u8, line, " \t");
                    // Skip comments
                    if (std.mem.startsWith(u8, trimmed, "//")) {
                        line_number += 1;
                        line_start = i + 1;
                        continue;
                    }

                    // Check for @enumFromInt(0) usage
                    if (std.mem.find(u8, line, "@enumFromInt(0)") != null) {
                        try violations.append(allocator, .{
                            .file_path = full_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                        });
                    }

                    line_number += 1;
                    line_start = i + 1;
                }
            }
        }
    }
};

/// Build step that checks for unused variable suppression patterns.
///
/// In this codebase, we don't use `_ = variable;` to suppress unused variable warnings.
/// Instead, we delete the unused variable/argument and update all call sites as necessary.
const CheckUnusedSuppressionStep = struct {
    step: Step,

    fn create(b: *std.Build) *CheckUnusedSuppressionStep {
        const self = b.allocator.create(CheckUnusedSuppressionStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "check-unused-suppression",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, _: Step.MakeOptions) !void {
        const b = step.owner;
        const allocator = b.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Scan all src/ directories for .zig files
        const io = step.owner.graph.io;
        var dir = std.Io.Dir.cwd().openDir(io, "src", .{ .iterate = true }) catch |err| {
            return step.fail("Failed to open src/ directory: {}", .{err});
        };
        defer dir.close(io);

        try scanDirectoryForUnusedSuppression(allocator, io, dir, "src", &violations);

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("UNUSED VARIABLE SUPPRESSION DETECTED\n", .{});
            std.debug.print("=" ** 80 ++ "\n\n", .{});

            std.debug.print(
                \\In this codebase, we do NOT use `_ = variable;` to suppress unused warnings.
                \\
                \\Instead, you should:
                \\  1. Delete the unused variable, parameter, or argument
                \\  2. Update all call sites as necessary
                \\  3. Propagate the change through the codebase until tests pass
                \\
                \\VIOLATIONS FOUND:
                \\
            , .{});

            for (violations.items) |violation| {
                std.debug.print("  {s}:{d}: {s}\n", .{
                    violation.file_path,
                    violation.line_number,
                    violation.line_content,
                });
            }

            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});

            return step.fail(
                "Found {d} unused variable suppression patterns (`_ = identifier;`). " ++
                    "Delete the unused variables and update call sites instead.",
                .{violations.items.len},
            );
        }
    }

    const Violation = struct {
        file_path: []const u8,
        line_number: usize,
        line_content: []const u8,
    };

    fn scanDirectoryForUnusedSuppression(
        allocator: std.mem.Allocator,
        io: std.Io,
        dir: std.Io.Dir,
        path_prefix: []const u8,
        violations: *std.ArrayList(Violation),
    ) !void {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next(io)) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

            const content = dir.readFileAlloc(io, entry.path, allocator, .limited(10 * 1024 * 1024)) catch continue;
            defer allocator.free(content);

            // Vendored Zig-compiler files carry upstream idioms this check would
            // flag (e.g. `_ =` suppressions in unimplemented TODO stubs whose
            // signatures are fixed by their callers); exempt them.
            if (std.mem.find(u8, content, vendored_zig_marker) != null) continue;

            var line_number: usize = 1;
            var line_start: usize = 0;

            for (content, 0..) |char, i| {
                if (char == '\n') {
                    const line = content[line_start..i];
                    const trimmed = std.mem.trim(u8, line, " \t");

                    // Check for pattern: _ = identifier;
                    // where identifier is alphanumeric with underscores
                    if (isUnusedSuppression(trimmed)) {
                        try violations.append(allocator, .{
                            .file_path = full_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                        });
                    }

                    line_number += 1;
                    line_start = i + 1;
                }
            }
        }
    }

    fn isUnusedSuppression(line: []const u8) bool {
        // Pattern: `_ = identifier;` where identifier is alphanumeric with underscores
        // Must start with "_ = " and end with ";"
        if (!std.mem.startsWith(u8, line, "_ = ")) return false;
        if (!std.mem.endsWith(u8, line, ";")) return false;

        // Extract the identifier part (between "_ = " and ";")
        const identifier = line[4 .. line.len - 1];

        // Must have at least one character
        if (identifier.len == 0) return false;

        // Check that identifier contains only alphanumeric chars and underscores
        // Also allow dots for field access like `_ = self.field;` which we also want to catch
        for (identifier) |c| {
            if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '.') {
                return false;
            }
        }

        return true;
    }
};

/// Build step that checks for deleted post-check architecture APIs being reintroduced.
///
/// This enforces the cor-style lowering contract:
/// - no output/canonicalization layer in post-check lowering
/// - no workspace/source-var remapping layer in monotype
/// - no canonical-source specialization lookup in compilation stages
const CheckPostcheckArchitectureStep = struct {
    step: Step,

    fn create(b: *std.Build) *CheckPostcheckArchitectureStep {
        const self = b.allocator.create(CheckPostcheckArchitectureStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "check-postcheck-architecture",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, _: Step.MakeOptions) !void {
        const b = step.owner;

        if (builtin.os.tag == .windows) {
            std.debug.print("Skipping post-check architecture check on Windows (perl not available)\n", .{});
            return;
        }

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        try child_argv.append(b.allocator, "perl");
        try child_argv.append(b.allocator, "ci/check_postcheck_architecture.pl");

        const io = b.graph.io;
        var child = try std.process.spawn(io, .{
            .argv = child_argv.items,
            .environ_map = &b.graph.environ_map,
        });
        const term = try child.wait(io);

        switch (term) {
            .exited => |code| {
                if (code != 0) {
                    return step.fail(
                        "Post-check architecture check failed. Run 'perl ci/check_postcheck_architecture.pl' to see details.",
                        .{},
                    );
                }
            },
            else => {
                return step.fail("ci/check_postcheck_architecture.pl terminated abnormally", .{});
            },
        }
    }
};

/// Build step that checks for @panic and std.debug.panic usage in interpreter and builtins.
///
/// In Roc's design philosophy, compile-time errors become runtime errors with helpful messages.
/// Users can run apps despite errors, and we provide actionable feedback. Using @panic unwinds
/// the stack and prevents showing helpful error messages.
///
/// Additionally, in WASM builds, @panic compiles to the `unreachable` instruction with no
/// message output, making debugging impossible. All runtime code must use roc_ops.crash()
/// to ensure error messages are properly displayed.
const CheckPanicStep = struct {
    step: Step,

    // Files to scan individually
    const scan_files = [_][]const u8{
        "src/eval/interpreter.zig",
    };

    // Directories to scan (all .zig files within)
    const scan_dirs = [_][]const u8{
        "src/builtins",
    };

    // Files to exclude from scanning (test-only files)
    const excluded_files = [_][]const u8{
        "fuzz_sort.zig",
    };

    // Line-level allowlist patterns - if any of these appear on the line, allow the @panic
    const allowlist_patterns = [_][]const u8{
        "trace_modules", // traceDbg helper in interpreter
    };

    // File-specific line ranges to exclude (test-only code)
    // Format: { file_suffix, start_line, end_line }
    const ExcludedRange = struct { file: []const u8, start: usize, end: usize };
    const excluded_ranges = [_]ExcludedRange{
        // TestEnv struct in utils.zig is test-only (lines 60-214)
        .{ .file = "utils.zig", .start = 60, .end = 214 },
        // Cross-module name matching in Check.zig requires string comparison (lines 5530-5547)
        // This is necessary because origin_module is an ident from the type's defining module,
        // while module_name is from the importing module's ident store - no way to compare without strings
        .{ .file = "Check.zig", .start = 5530, .end = 5547 },
    };

    fn create(b: *std.Build) *CheckPanicStep {
        const self = b.allocator.create(CheckPanicStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "check-panic-usage",
                .owner = b,
                .makeFn = makePanic,
            }),
        };
        return self;
    }

    fn isExcludedFile(file_name: []const u8) bool {
        for (excluded_files) |excluded| {
            if (std.mem.eql(u8, file_name, excluded)) return true;
        }
        return false;
    }

    fn isAllowlisted(line: []const u8) bool {
        for (allowlist_patterns) |pattern| {
            if (std.mem.find(u8, line, pattern) != null) return true;
        }
        return false;
    }

    fn isInExcludedRange(file_path: []const u8, line_number: usize) bool {
        for (excluded_ranges) |range| {
            if (std.mem.endsWith(u8, file_path, range.file)) {
                if (line_number >= range.start and line_number <= range.end) {
                    return true;
                }
            }
        }
        return false;
    }

    fn scanFile(allocator: std.mem.Allocator, io: std.Io, file_path: []const u8, violations: *std.ArrayList(Violation)) !void {
        const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(50 * 1024 * 1024)) catch |err| {
            std.debug.print("Warning: Failed to read {s}: {}\n", .{ file_path, err });
            return;
        };
        defer allocator.free(content);

        var line_number: usize = 1;
        var line_start: usize = 0;

        for (content, 0..) |char, i| {
            if (char == '\n') {
                const line = content[line_start..i];
                const trimmed = std.mem.trim(u8, line, " \t");

                // Skip comments
                if (!std.mem.startsWith(u8, trimmed, "//")) {
                    // Check for @panic usage
                    const has_panic = std.mem.find(u8, line, "@panic(") != null;
                    // Check for std.debug.panic usage
                    const has_debug_panic = std.mem.find(u8, line, "std.debug.panic") != null;

                    if (has_panic or has_debug_panic) {
                        if (!isAllowlisted(line) and !isInExcludedRange(file_path, line_number)) {
                            try violations.append(allocator, .{
                                .file_path = try allocator.dupe(u8, file_path),
                                .line_number = line_number,
                                .line_content = try allocator.dupe(u8, trimmed),
                            });
                        }
                    }
                }

                line_number += 1;
                line_start = i + 1;
            }
        }
    }

    fn makePanic(step: *Step, _: Step.MakeOptions) !void {
        const b = step.owner;
        const allocator = b.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        const io = b.graph.io;

        // Scan individual files
        for (scan_files) |file_path| {
            try scanFile(allocator, io, file_path, &violations);
        }

        // Scan directories
        for (scan_dirs) |dir_path| {
            var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| {
                std.debug.print("Warning: Failed to open directory {s}: {}\n", .{ dir_path, err });
                continue;
            };
            defer dir.close(io);

            var iter = dir.iterate();
            while (try iter.next(io)) |entry| {
                if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".zig")) {
                    if (!isExcludedFile(entry.name)) {
                        const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
                        defer allocator.free(full_path);
                        try scanFile(allocator, io, full_path, &violations);
                    }
                }
            }
        }

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("FORBIDDEN PATTERN: @panic / std.debug.panic in runtime code\n", .{});
            std.debug.print("=" ** 80 ++ "\n\n", .{});

            std.debug.print(
                \\Using @panic or std.debug.panic is forbidden in interpreter and builtins.
                \\
                \\WHY THIS RULE EXISTS:
                \\  1. Roc's design philosophy is that compile-time errors become runtime errors with
                \\     helpful messages. Users can run apps despite errors, and we provide actionable
                \\     feedback. @panic unwinds the stack and prevents us from showing helpful errors.
                \\
                \\  2. In WASM builds, @panic compiles to the `unreachable` instruction with NO
                \\     message output, making debugging impossible.
                \\
                \\WHAT TO DO INSTEAD:
                \\  In interpreter.zig, use the triggerCrash() method:
                \\
                \\    self.triggerCrash("Description of the error", false, roc_ops);
                \\
                \\  In builtins, use roc_ops.crash():
                \\
                \\    roc_ops.crash("Description of the error");
                \\
                \\  For debug output, use roc_ops.dbg():
                \\
                \\    roc_ops.dbg("Debug message");
                \\
                \\VIOLATIONS FOUND:
                \\
            , .{});

            for (violations.items) |violation| {
                std.debug.print("  {s}:{d}: {s}\n", .{
                    violation.file_path,
                    violation.line_number,
                    violation.line_content,
                });
            }

            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});

            return step.fail(
                "Found {d} uses of @panic or std.debug.panic in runtime code. " ++
                    "Use roc_ops.crash() to report errors through the proper RocOps crash handler. " ++
                    "See above for details.",
                .{violations.items.len},
            );
        }
    }

    const Violation = struct {
        file_path: []const u8,
        line_number: usize,
        line_content: []const u8,
    };
};

/// Build step that checks for global stdio usage in CLI code.
///
/// The CLI code uses a context-based I/O pattern where stdout/stderr are accessed
/// through `ctx.io.stdout()` and `ctx.io.stderr()`. This prepares for Zig's upcoming
/// I/O interface changes where I/O is passed through functions (like Allocator).
///
/// This step enforces that pattern by failing the build if direct global stdio
/// access is found in src/cli/main.zig.
const CheckCliGlobalStdioStep = struct {
    step: Step,

    fn create(b: *std.Build) *CheckCliGlobalStdioStep {
        const self = b.allocator.create(CheckCliGlobalStdioStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "check-cli-global-stdio",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, _: Step.MakeOptions) !void {
        const b = step.owner;
        const allocator = b.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Only scan src/cli/main.zig
        const file_path = "src/cli/main.zig";
        const io = step.owner.graph.io;
        const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(10 * 1024 * 1024)) catch |err| {
            return step.fail("Failed to read {s}: {}", .{ file_path, err });
        };
        defer allocator.free(content);

        var line_number: usize = 1;
        var line_start: usize = 0;

        for (content, 0..) |char, i| {
            if (char == '\n') {
                const line = content[line_start..i];
                const trimmed = std.mem.trim(u8, line, " \t");

                // Check for forbidden patterns that indicate global stdio usage
                // These patterns bypass ctx.io and use global state
                const forbidden_patterns = [_][]const u8{
                    "std.io.getStdOut()",
                    "std.io.getStdErr()",
                    "std.fs.File.stdout()",
                    "std.fs.File.stderr()",
                };

                for (forbidden_patterns) |pattern| {
                    if (std.mem.find(u8, trimmed, pattern) != null) {
                        try violations.append(allocator, .{
                            .file_path = file_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                            .pattern = pattern,
                        });
                    }
                }

                line_number += 1;
                line_start = i + 1;
            }
        }

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("GLOBAL STDIO USAGE DETECTED IN CLI\n", .{});
            std.debug.print("=" ** 80 ++ "\n\n", .{});

            std.debug.print(
                \\In the CLI code, we use context-based I/O, not global stdio functions.
                \\
                \\WHY THIS RULE EXISTS:
                \\  1. TESTABILITY: Context-based I/O allows tests to inject mock writers
                \\     to capture and verify output.
                \\
                \\  2. FUTURE COMPATIBILITY: Zig's upcoming I/O interface will pass I/O
                \\     through functions (like Allocator). Using ctx.io prepares us for this.
                \\
                \\  3. CONSISTENCY: All CLI functions receive ctx which contains allocators
                \\     and I/O. This provides a uniform interface for resources.
                \\
                \\WHAT TO DO INSTEAD:
                \\  Access stdout/stderr through the CliCtx:
                \\
                \\  Example - WRONG:
                \\    const stdout = std.io.getStdOut().writer();
                \\    const stderr = std.fs.File.stderr().writer();
                \\
                \\  Example - RIGHT:
                \\    const stdout = ctx.io.stdout();
                \\    const stderr = ctx.io.stderr();
                \\
                \\VIOLATIONS FOUND:
                \\
            , .{});

            for (violations.items) |violation| {
                std.debug.print("  {s}:{d}: found `{s}` in: {s}\n", .{
                    violation.file_path,
                    violation.line_number,
                    violation.pattern,
                    violation.line_content,
                });
            }

            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});

            return step.fail(
                "Found {d} global stdio usage(s) in CLI code. " ++
                    "Use ctx.io.stdout() and ctx.io.stderr() instead.",
                .{violations.items.len},
            );
        }
    }

    const Violation = struct {
        file_path: []const u8,
        line_number: usize,
        line_content: []const u8,
        pattern: []const u8,
    };
};

/// Build step that parses kcov JSON output and prints coverage summary.
/// Used by the `coverage` build step to report parser code coverage statistics.
const CoverageSummaryStep = struct {
    step: Step,
    coverage_dir: []const u8,
    exe_name: []const u8,
    label: []const u8,
    min_coverage: f64,

    /// Coverage is supported on:
    /// - macOS (ARM64 and x86_64): Uses libdwarf for DWARF parsing
    /// - Linux ARM64: Uses libdw (elfutils) for DWARF parsing
    ///
    /// TODO ZIG 16: re-check if this DWARF bug is fixed in 0.16 — may be able to enable x86_64 coverage
    /// Coverage does NOT work on Linux x86_64 due to a Zig 0.15.2 compiler bug that
    /// generates invalid DWARF .debug_line sections. libdw fails with "invalid
    /// .debug_line section" when parsing user code compilation units, while stdlib
    /// CUs parse successfully. This causes kcov to find only stdlib files, not user
    /// source files. ARM64 Zig generates valid DWARF, so coverage works there.
    /// See: https://github.com/roc-lang/roc/pull/8864 for investigation details.
    fn create(b: *std.Build, coverage_dir: []const u8, exe_name: []const u8) *CoverageSummaryStep {
        return createWithOptions(b, coverage_dir, exe_name, "PARSER", 28.0);
    }

    fn createWithOptions(b: *std.Build, coverage_dir: []const u8, exe_name: []const u8, label: []const u8, min_coverage: f64) *CoverageSummaryStep {
        const self = b.allocator.create(CoverageSummaryStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "coverage-summary",
                .owner = b,
                .makeFn = make,
            }),
            .coverage_dir = coverage_dir,
            .exe_name = exe_name,
            .label = label,
            .min_coverage = min_coverage,
        };
        return self;
    }

    fn make(step: *Step, _: Step.MakeOptions) !void {
        const b = step.owner;
        const allocator = b.allocator;
        const self: *CoverageSummaryStep = @fieldParentPtr("step", step);

        // Read kcov JSON output
        // kcov creates a subdirectory named after the executable (e.g., parse_unit_coverage/)
        // which contains the coverage.json file
        const json_path = try std.fmt.allocPrint(allocator, "{s}/{s}/coverage.json", .{ self.coverage_dir, self.exe_name });
        defer allocator.free(json_path);

        const io = b.graph.io;
        const json_content = std.Io.Dir.cwd().readFileAlloc(io, json_path, allocator, .limited(10 * 1024 * 1024)) catch |err| {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            std.debug.print("COVERAGE ERROR\n", .{});
            std.debug.print("=" ** 60 ++ "\n\n", .{});
            std.debug.print("Could not open coverage JSON at {s}: {}\n", .{ json_path, err });
            std.debug.print("\nMake sure kcov is installed:\n", .{});
            std.debug.print("  - Linux: apt install kcov\n", .{});
            std.debug.print("  - macOS: brew install kcov\n\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            return;
        };
        defer allocator.free(json_content);

        // Parse and summarize coverage
        const result = try parseCoverageJson(allocator, json_content, self.label, self.coverage_dir);

        // Fail if kcov didn't capture any data - this indicates a problem with kcov
        if (result.total_lines == 0) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            std.debug.print("COVERAGE ERROR: NO DATA CAPTURED\n", .{});
            std.debug.print("=" ** 60 ++ "\n\n", .{});
            std.debug.print("kcov reported 0 total lines - coverage data was not captured.\n", .{});
            std.debug.print("This indicates a problem with kcov or the binary format.\n\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            return step.fail("kcov failed to capture coverage data (0 total lines)", .{});
        }

        // Enforce minimum coverage threshold
        if (result.percent < self.min_coverage) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            std.debug.print("COVERAGE CHECK FAILED\n", .{});
            std.debug.print("=" ** 60 ++ "\n\n", .{});
            std.debug.print("{s} coverage is {d:.2}%, minimum required is {d:.2}%\n", .{ self.label, result.percent, self.min_coverage });
            std.debug.print("Add more tests to improve coverage before merging.\n\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            return step.fail("{s} coverage {d:.2}% is below minimum {d:.2}%", .{ self.label, result.percent, self.min_coverage });
        }
    }

    const CoverageResult = struct {
        percent: f64,
        total_lines: u64,
    };

    fn parseCoverageJson(allocator: std.mem.Allocator, json_content: []const u8, label: []const u8, coverage_dir: []const u8) !CoverageResult {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_content, .{});
        defer parsed.deinit();

        const root = parsed.value;

        // Get totals from root level (these are integers)
        const total_lines: u64 = blk: {
            const val = root.object.get("total_lines") orelse break :blk 0;
            if (val != .integer) break :blk 0;
            break :blk @intCast(val.integer);
        };
        const covered_lines: u64 = blk: {
            const val = root.object.get("covered_lines") orelse break :blk 0;
            if (val != .integer) break :blk 0;
            break :blk @intCast(val.integer);
        };

        // Collect uncovered files for the summary
        var uncovered_files = std.ArrayList(UncoveredFile).empty;
        defer {
            for (uncovered_files.items) |uf| {
                allocator.free(uf.file);
            }
            uncovered_files.deinit(allocator);
        }

        // kcov JSON format has "files" array with file coverage data
        if (root.object.get("files")) |files_val| {
            if (files_val == .array) {
                for (files_val.array.items) |file_obj| {
                    if (file_obj != .object) continue;

                    const filename_val = file_obj.object.get("file") orelse continue;
                    if (filename_val != .string) continue;
                    const filename = filename_val.string;

                    // Only include src/parse files
                    if (std.mem.find(u8, filename, "src/parse") == null) continue;

                    // Skip test files
                    if (std.mem.find(u8, filename, "/test/") != null) continue;

                    // Get coverage percentage (stored as string in kcov JSON)
                    const percent_val = file_obj.object.get("percent_covered") orelse continue;
                    if (percent_val != .string) continue;

                    const covered_str = file_obj.object.get("covered_lines") orelse continue;
                    const total_str = file_obj.object.get("total_lines") orelse continue;
                    if (covered_str != .string or total_str != .string) continue;

                    const file_covered = std.fmt.parseInt(u64, covered_str.string, 10) catch 0;
                    const file_total = std.fmt.parseInt(u64, total_str.string, 10) catch 0;
                    const file_uncovered = file_total - file_covered;

                    if (file_uncovered > 0) {
                        try uncovered_files.append(allocator, .{
                            .file = try allocator.dupe(u8, filename),
                            .uncovered_lines = file_uncovered,
                            .total_lines = file_total,
                            .percent = std.fmt.parseFloat(f64, percent_val.string) catch 0.0,
                        });
                    }
                }
            }
        }

        // Print summary
        const uncovered_lines = total_lines - covered_lines;
        const percent = if (total_lines > 0)
            @as(f64, @floatFromInt(covered_lines)) / @as(f64, @floatFromInt(total_lines)) * 100.0
        else
            0.0;

        std.debug.print("\n", .{});
        std.debug.print("=" ** 60 ++ "\n", .{});
        std.debug.print("{s} CODE COVERAGE SUMMARY\n", .{label});
        std.debug.print("=" ** 60 ++ "\n\n", .{});

        std.debug.print("Total lines:     {d}\n", .{total_lines});
        std.debug.print("Covered lines:   {d}\n", .{covered_lines});
        std.debug.print("Uncovered lines: {d}\n", .{uncovered_lines});
        std.debug.print("Coverage:        {d:.2}%\n\n", .{percent});

        if (uncovered_files.items.len > 0) {
            std.debug.print("Files with uncovered lines:\n", .{});

            // Sort by uncovered lines (descending) for prioritization
            std.mem.sort(UncoveredFile, uncovered_files.items, {}, struct {
                fn lessThan(_: void, a: UncoveredFile, b: UncoveredFile) bool {
                    return a.uncovered_lines > b.uncovered_lines; // Descending
                }
            }.lessThan);

            for (uncovered_files.items) |uf| {
                // Extract just the filename from the full path
                const basename = std.fs.path.basename(uf.file);
                std.debug.print("  {s}: {d:.1}% covered ({d}/{d} lines uncovered)\n", .{
                    basename,
                    uf.percent,
                    uf.uncovered_lines,
                    uf.total_lines,
                });
            }
        }

        std.debug.print("\n" ++ "=" ** 60 ++ "\n", .{});
        std.debug.print("Full HTML report: {s}/index.html\n", .{coverage_dir});
        std.debug.print("=" ** 60 ++ "\n", .{});

        return .{ .percent = percent, .total_lines = total_lines };
    }

    const UncoveredFile = struct {
        file: []const u8,
        uncovered_lines: u64,
        total_lines: u64,
        percent: f64,
    };
};

fn checkFxPlatformTestCoverage(step: *Step) !void {
    const b = step.owner;
    std.debug.print("---- checking fx platform test coverage ----\n", .{});

    const allocator = b.allocator;

    // Get all .roc files in test/fx (excluding subdirectories)
    const io = b.graph.io;
    var fx_dir = try std.Io.Dir.cwd().openDir(io, "test/fx", .{ .iterate = true });
    defer fx_dir.close(io);

    var roc_files = std.ArrayList([]const u8).empty;
    defer {
        for (roc_files.items) |file| {
            allocator.free(file);
        }
        roc_files.deinit(allocator);
    }

    var dir_iter = fx_dir.iterate();
    while (try dir_iter.next(io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            const file_name = try allocator.dupe(u8, entry.name);
            try roc_files.append(allocator, file_name);
        }
    }

    // Sort the list for consistent output
    std.mem.sort([]const u8, roc_files.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    // Find all references to test/fx/*.roc files in test source files
    var tested_files = std.StringHashMap(void).init(allocator);
    defer {
        var key_iter = tested_files.keyIterator();
        while (key_iter.next()) |key| {
            allocator.free(key.*);
        }
        tested_files.deinit();
    }

    // Scan both the test file and the shared specs file
    const test_files_to_scan = [_][]const u8{
        "src/cli/test/fx_platform_test.zig",
        "src/cli/test/fx_test_specs.zig",
    };

    for (test_files_to_scan) |test_file_path| {
        const test_file_contents = std.Io.Dir.cwd().readFileAlloc(io, test_file_path, allocator, .limited(1024 * 1024)) catch |err| {
            std.debug.print("Warning: Could not read {s}: {}\n", .{ test_file_path, err });
            continue;
        };
        defer allocator.free(test_file_contents);

        var line_iter = std.mem.splitScalar(u8, test_file_contents, '\n');
        while (line_iter.next()) |line| {
            // Look for patterns like "test/fx/filename.roc"
            var search_start: usize = 0;
            while (std.mem.findPos(u8, line, search_start, "test/fx/")) |idx| {
                const rest_of_line = line[idx..];
                // Find the end of the filename
                if (std.mem.find(u8, rest_of_line, ".roc")) |roc_pos| {
                    const full_path = rest_of_line[0 .. roc_pos + 4]; // Include ".roc"
                    // Extract just the filename (after "test/fx/")
                    const filename = full_path["test/fx/".len..];
                    // Only count files in test/fx (not subdirectories like test/fx/subdir/)
                    if (std.mem.find(u8, filename, "/") == null) {
                        // Dupe the filename since the source buffer will be freed
                        const duped_filename = try allocator.dupe(u8, filename);
                        try tested_files.put(duped_filename, {});
                    }
                }
                search_start = idx + 1;
            }
        }
    }

    // Find missing tests
    var missing_tests = std.ArrayList([]const u8).empty;
    defer missing_tests.deinit(allocator);

    for (roc_files.items) |roc_file| {
        if (!tested_files.contains(roc_file)) {
            try missing_tests.append(allocator, roc_file);
        }
    }

    // Report results
    if (missing_tests.items.len > 0) {
        std.debug.print("\nERROR: The following .roc files in test/fx/ do not have tests:\n", .{});
        for (missing_tests.items) |missing_file| {
            std.debug.print("  - {s}\n", .{missing_file});
        }
        std.debug.print("\nPlease add tests in fx_platform_test.zig or fx_test_specs.zig, or remove these files from test/fx/.\n", .{});
        return step.fail("{d} .roc file(s) in test/fx/ are missing tests", .{missing_tests.items.len});
    }

    std.debug.print("All {d} .roc files in test/fx/ have tests.\n", .{roc_files.items.len});
}

const CheckFxStep = struct {
    step: Step,

    fn create(b: *std.Build) *CheckFxStep {
        const self = b.allocator.create(CheckFxStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "checkfx-inner",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;
        try checkFxPlatformTestCoverage(step);
    }
};

const BuiltinCompilerRun = struct {
    run: *Step.Run,
    builtin_bin: std.Build.LazyPath,
    builtin_indices_bin: std.Build.LazyPath,
};

fn createAndRunBuiltinCompiler(
    b: *std.Build,
    roc_modules: modules.RocModules,
    flag_enable_tracy: ?[]const u8,
    roc_files: []const []const u8,
) BuiltinCompilerRun {
    // Build and run the compiler
    const builtin_compiler_exe = b.addExecutable(.{
        .name = "builtin_compiler",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/builtin_compiler/main.zig"),
            .target = b.graph.host, // this runs at build time on the *host* machine!
            .optimize = .Debug, // No need to optimize - only compiles builtin modules
            // ctx.CoreCtx reads env vars via std.c.getenv; Zig 0.16 requires
            // link_libc=true on any compile unit that references std.c.*.
            // (add_tracy below also sets this when tracy is enabled, but tracy is
            // always disabled for the build-time builtin compiler.)
            .link_libc = true,
        }),
    });
    configureBackend(builtin_compiler_exe, b.graph.host);

    // Add only the minimal modules needed for parsing/checking
    builtin_compiler_exe.root_module.addImport("base", roc_modules.base);
    builtin_compiler_exe.root_module.addImport("collections", roc_modules.collections);
    builtin_compiler_exe.root_module.addImport("types", roc_modules.types);
    builtin_compiler_exe.root_module.addImport("parse", roc_modules.parse);
    builtin_compiler_exe.root_module.addImport("can", roc_modules.can);
    builtin_compiler_exe.root_module.addImport("check", roc_modules.check);
    builtin_compiler_exe.root_module.addImport("reporting", roc_modules.reporting);
    builtin_compiler_exe.root_module.addImport("builtins", roc_modules.builtins);

    // Add tracy support (required by parse/can/check modules)
    add_tracy(b, roc_modules.build_options, builtin_compiler_exe, b.graph.host, false, flag_enable_tracy);

    // Run the builtin compiler to generate .bin files in zig-out/builtins/
    const run_builtin_compiler = b.addRunArtifact(builtin_compiler_exe);

    // Add all .roc files as explicit file inputs so Zig's cache tracks them
    for (roc_files) |roc_path| {
        run_builtin_compiler.addFileArg(b.path(roc_path));
    }

    const builtin_bin = run_builtin_compiler.addOutputFileArg("Builtin.bin");
    const builtin_indices_bin = run_builtin_compiler.addOutputFileArg("builtin_indices.bin");

    return .{
        .run = run_builtin_compiler,
        .builtin_bin = builtin_bin,
        .builtin_indices_bin = builtin_indices_bin,
    };
}

fn createTestPlatformHostLib(
    b: *std.Build,
    name: []const u8,
    host_path: []const u8,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
    options: TestHostOptions,
) *Step.Compile {
    const lib = b.addLibrary(.{
        .name = name,
        .linkage = .static,
        .root_module = b.createModule(.{
            .root_source_file = b.path(host_path),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true, // Enable Position Independent Code for PIE compatibility
            // Only linked so host code can set up stack overflow handling.
            .link_libc = testHostNeedsLibc(options, target),
        }),
    });
    configureBackend(lib, target);
    if (options.uses_stack_handler) {
        lib.root_module.addImport("base", roc_modules.base);
    }
    lib.root_module.addImport("builtins", roc_modules.builtins);
    lib.root_module.addImport("build_options", roc_modules.build_options);
    lib.root_module.addImport("shim_io", b.addModule("shim_io", .{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    // Bundle compiler_rt when the generated host object may call compiler_rt
    // routines that are not supplied by the OS libraries. ARM64 Windows Zig code
    // can emit stack-protector calls to __stack_chk_fail; x86_64 macOS (LLVM)
    // needs symbols like __zig_probe_stack.
    lib.bundle_compiler_rt = testHostNeedsCompilerRt(target);

    return lib;
}

/// Builds a test platform host library and sets up a step to copy it to the target-specific directory.
/// Returns the final step for dependency wiring.
fn buildAndCopyTestPlatformHostLib(
    b: *std.Build,
    platform_dir: []const u8,
    target: ResolvedTarget,
    target_name: []const u8,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
) *Step {
    const options = TestHostOptions{
        .uses_stack_handler = testPlatformUsesStackHandler(platform_dir),
    };

    const lib = createTestPlatformHostLib(
        b,
        b.fmt("test_platform_{s}_host_{s}", .{ platform_dir, target_name }),
        b.pathJoin(&.{ "test", platform_dir, "platform/host.zig" }),
        target,
        optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
        options,
    );

    // Use correct filename for target platform
    const host_filename = if (target.result.os.tag == .windows) "host.lib" else "libhost.a";
    const archive_path = b.pathJoin(&.{ "test", platform_dir, "platform/targets", target_name, host_filename });

    const copy_step = b.addUpdateSourceFiles();
    copy_step.addCopyFileToSource(lib.getEmittedBin(), archive_path);

    // Workaround for Zig bug https://codeberg.org/ziglang/zig/issues/30572
    // Zig's archive generator doesn't add the required padding byte after odd-sized
    // members, causing lld to reject the archive with:
    //   "Archive::children failed: truncated or malformed archive"
    if (target.result.os.tag != .windows) {
        const fix_step = FixArchivePaddingStep.create(b, archive_path);
        fix_step.step.dependOn(&copy_step.step);
        return &fix_step.step;
    }

    return &copy_step.step;
}

// Custom step to remove a directory tree (replaces removed addRemoveDirTree)
const RemoveDirTreeStep = struct {
    step: Step,
    dir_path: []const u8,

    fn create(b: *std.Build, dir_path: []const u8) *RemoveDirTreeStep {
        const self = b.allocator.create(RemoveDirTreeStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "remove-dir-tree",
                .owner = b,
                .makeFn = make,
            }),
            .dir_path = dir_path,
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;
        const self: *RemoveDirTreeStep = @fieldParentPtr("step", step);
        const io = step.owner.graph.io;
        std.Io.Dir.cwd().deleteTree(io, self.dir_path) catch {};
    }
};

/// Build the wasm test platform host as a relocatable .wasm object (not an archive).
/// Surgical linking operates on a single relocatable object with linking/reloc sections.
fn buildAndCopyWasmHostObject(
    b: *std.Build,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
) *Step {
    const obj = b.addObject(.{
        .name = "host",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/wasm/platform/host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    configureBackend(obj, target);
    obj.root_module.addImport("builtins", roc_modules.builtins);
    obj.root_module.addImport("build_options", roc_modules.build_options);

    const dest_path = "test/wasm/platform/targets/wasm32/host.wasm";
    const copy_step = b.addUpdateSourceFiles();
    copy_step.addCopyFileToSource(obj.getEmittedBin(), dest_path);

    return &copy_step.step;
}

// Workaround for Zig bug https://codeberg.org/ziglang/zig/issues/30572
const FixArchivePaddingStep = struct {
    step: Step,
    archive_path: []const u8,

    fn create(b: *std.Build, archive_path: []const u8) *FixArchivePaddingStep {
        const self = b.allocator.create(FixArchivePaddingStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "fix-archive-padding",
                .owner = b,
                .makeFn = make,
            }),
            .archive_path = archive_path,
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;
        const self: *FixArchivePaddingStep = @fieldParentPtr("step", step);
        const io = step.owner.graph.io;

        const file = std.Io.Dir.cwd().openFile(io, self.archive_path, .{ .mode = .read_write }) catch {
            // Archive doesn't exist yet (e.g. cross-compilation target not built) — skip silently.
            return;
        };
        defer file.close(io);

        const stat = try file.stat(io);
        var file_size = stat.size;

        // AR format requires archives to end on an even byte boundary.
        // If file size is odd, append a newline padding byte.
        // This fixes Zig bug https://codeberg.org/ziglang/zig/issues/30572
        // where Zig's archiver doesn't add required padding after odd-sized members.
        if (file_size % 2 == 1) {
            try file.writePositionalAll(io, "\n", file_size);
            file_size += 1;
        }

        // Parse the archive to verify member offsets are valid.
        // This catches cases where lld would fail with "truncated or malformed archive".
        var header_buf: [8]u8 = undefined;
        _ = try file.readPositionalAll(io, &header_buf, 0);
        if (!std.mem.eql(u8, &header_buf, "!<arch>\n")) {
            std.debug.print("Warning: Invalid archive magic in {s}\n", .{self.archive_path});
            return;
        }

        var offset: u64 = 8; // After magic
        while (offset + 60 <= file_size) {
            var size_buf: [10]u8 = undefined;
            _ = try file.readPositionalAll(io, &size_buf, offset + 48); // Read size field (offset 48 within 60-byte header)

            // Parse size (ASCII decimal, space-padded)
            var size: u64 = 0;
            for (size_buf) |c| {
                if (c >= '0' and c <= '9') {
                    size = size * 10 + (c - '0');
                } else break;
            }

            // Move to next member (header + content + padding if odd)
            offset += 60 + size;
            if (size % 2 == 1) {
                offset += 1; // Padding byte expected
            }

            // If we're exactly at EOF, archive is valid
            if (offset == file_size) break;

            // If next offset would be past EOF, we have a problem - add missing padding
            if (offset > file_size) {
                const missing = offset - file_size;
                const padding = "\n\n"; // At most 1 byte needed, but be safe
                try file.writePositionalAll(io, padding[0..@min(missing, 2)], file_size);
                break;
            }
        }
    }
};

/// Custom build step that clears the Roc cache directory.
/// Uses Zig's native filesystem APIs for cross-platform support.
const ClearRocCacheStep = struct {
    step: Step,

    fn create(b: *std.Build) *ClearRocCacheStep {
        const self = b.allocator.create(ClearRocCacheStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "clear-roc-cache",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;

        const b = step.owner;
        const allocator = b.allocator;
        const io = b.graph.io;

        // Get the cache directory path using the same logic as cache_config.zig
        const cache_dir = getCacheDir(allocator, b.graph.environ_map) catch |err| {
            std.debug.print("Warning: Could not determine cache directory: {s}\n", .{@errorName(err)});
            return;
        };
        defer allocator.free(cache_dir);

        // Check if cache directory exists before trying to delete
        std.Io.Dir.cwd().access(io, cache_dir, .{}) catch {
            // Cache doesn't exist, nothing to do
            std.debug.print("Roc cache not found (nothing to clear)\n", .{});
            return;
        };

        // Try to delete the cache directory
        std.Io.Dir.cwd().deleteTree(io, cache_dir) catch |err| {
            std.debug.print("Warning: Could not clear cache at {s}: {s}\n", .{ cache_dir, @errorName(err) });
            return;
        };

        std.debug.print("Cleared roc cache at {s}\n", .{cache_dir});
    }

    /// Get the Roc cache directory path (matches cache_config.zig logic)
    fn getCacheDir(allocator: std.mem.Allocator, environ_map: std.process.Environ.Map) ![]u8 {
        const cache_dir_name = switch (builtin.os.tag) {
            .windows => "Roc",
            else => "roc",
        };

        // Respect XDG_CACHE_HOME if set
        if (environ_map.get("XDG_CACHE_HOME")) |xdg_cache| {
            return std.fs.path.join(allocator, &[_][]const u8{ xdg_cache, cache_dir_name });
        } else {
            // Fall back to platform defaults
            const home_env = switch (builtin.os.tag) {
                .windows => "APPDATA",
                else => "HOME",
            };

            const home_dir = environ_map.get(home_env) orelse {
                return error.NoHomeDirectory;
            };

            return switch (builtin.os.tag) {
                .linux => std.fs.path.join(allocator, &[_][]const u8{ home_dir, ".cache", cache_dir_name }),
                .macos => std.fs.path.join(allocator, &[_][]const u8{ home_dir, "Library", "Caches", cache_dir_name }),
                .windows => std.fs.path.join(allocator, &[_][]const u8{ home_dir, cache_dir_name }),
                else => std.fs.path.join(allocator, &[_][]const u8{ home_dir, ".cache", cache_dir_name }),
            };
        }
    }
};

const PrintBuildSuccessStep = struct {
    step: Step,

    fn create(b: *std.Build) *PrintBuildSuccessStep {
        const self = b.allocator.create(PrintBuildSuccessStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "print-build-success",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = step;
        _ = options;
        std.debug.print("Build succeeded!\n", .{});
    }
};

/// Create a step that clears the Roc cache directory.
/// This is useful when rebuilding test platforms to ensure stale cached hosts aren't used.
fn createClearCacheStep(b: *std.Build) *Step {
    const clear_cache = ClearRocCacheStep.create(b);
    return &clear_cache.step;
}

fn setupTestPlatforms(
    b: *std.Build,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    build_test_hosts_step: *Step,
    strip: bool,
    omit_frame_pointer: ?bool,
    platform_filter: ?[]const u8,
) *Step {
    // Clear the Roc cache when test platforms are rebuilt to ensure stale cached hosts aren't used
    const clear_cache_step = createClearCacheStep(b);
    const native_target_name = roc_target.RocTarget.fromStdTarget(target.result).toName();

    // Build all test platforms for native target
    for (all_test_platform_dirs) |platform_dir| {
        if (platform_filter) |filter| {
            if (!std.mem.eql(u8, platform_dir, filter)) continue;
        }
        const copy_step = buildAndCopyTestPlatformHostLib(
            b,
            platform_dir,
            target,
            native_target_name,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
        );
        clear_cache_step.dependOn(copy_step);
    }

    // Cross-compile for musl targets (glibc is not needed for native CLI platform tests)
    for (musl_cross_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        for (all_test_platform_dirs) |platform_dir| {
            if (platform_filter) |filter| {
                if (!std.mem.eql(u8, platform_dir, filter)) continue;
            }
            const copy_step = buildAndCopyTestPlatformHostLib(
                b,
                platform_dir,
                cross_resolved_target,
                cross_target.name,
                optimize,
                roc_modules,
                strip,
                omit_frame_pointer,
            );
            clear_cache_step.dependOn(copy_step);
        }
    }

    // Cross-compile for Windows targets
    for (windows_cross_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        for (all_test_platform_dirs) |platform_dir| {
            if (platform_filter) |filter| {
                if (!std.mem.eql(u8, platform_dir, filter)) continue;
            }
            const copy_step = buildAndCopyTestPlatformHostLib(
                b,
                platform_dir,
                cross_resolved_target,
                cross_target.name,
                optimize,
                roc_modules,
                strip,
                omit_frame_pointer,
            );
            clear_cache_step.dependOn(copy_step);
        }
    }

    // Build the wasm test platform host as a relocatable .wasm object for surgical linking
    const wasm_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none });
    const wasm_host_step = buildAndCopyWasmHostObject(
        b,
        wasm_target,
        optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
    );
    clear_cache_step.dependOn(wasm_host_step);

    b.getInstallStep().dependOn(clear_cache_step);
    build_test_hosts_step.dependOn(clear_cache_step);

    return wasm_host_step;
}

fn configureZigCacheEnvironment(b: *std.Build) void {
    const local_cache_dir = absoluteBuildPath(b, b.cache_root.path orelse ".");
    const global_cache_dir = absoluteBuildPath(b, b.graph.global_cache_root.path orelse ".");
    const temp_dir = b.pathJoin(&.{ local_cache_dir, "tmp" });

    std.Io.Dir.cwd().createDirPath(b.graph.io, temp_dir) catch |err| {
        std.debug.panic("unable to create build temp directory '{s}': {s}", .{ temp_dir, @errorName(err) });
    };

    b.graph.environ_map.put("ZIG_LOCAL_CACHE_DIR", local_cache_dir) catch @panic("OOM");
    b.graph.environ_map.put("ZIG_GLOBAL_CACHE_DIR", global_cache_dir) catch @panic("OOM");
    b.graph.environ_map.put("TEMP", temp_dir) catch @panic("OOM");
    b.graph.environ_map.put("TMP", temp_dir) catch @panic("OOM");
    b.graph.environ_map.put("TMPDIR", temp_dir) catch @panic("OOM");
}

fn absoluteBuildPath(b: *std.Build, path: []const u8) []const u8 {
    if (std.fs.path.isAbsolute(path)) return path;
    return b.pathFromRoot(path);
}

pub fn build(b: *std.Build) void {
    configureZigCacheEnvironment(b);

    // Ensure zig-out/bin exists — Zig's install step can silently fail after `rm -rf zig-out`
    std.Io.Dir.cwd().createDirPath(b.graph.io, "zig-out/bin") catch {};

    // Build/run split used by MiniCI:
    // - `build-*` steps own compile, install, generation, and prep work.
    // - `run-*` steps own execution of checks/tests/tools after prep is done.
    // - If a `run-*` step needs a binary or generated input, wire that work into
    //   a `build-*` step and add it to `build-ci`.
    // - The user-facing step for building the Roc CLI is `roc`.
    // - Do not add duplicate alias steps. References should use the exact
    //   `roc`, `build-*`, `run-*`, `run-check-*`, or `run-test-*` step name.
    // - MiniCI runs `build-ci` once and then runs leaf `run-*` jobs. Keep
    //   aggregate steps out of MiniCI so each job remains independently
    //   reportable and re-runnable.
    // MiniCI intentionally does not parse Zig summaries to detect misplaced
    // build work; this convention is the source of truth.
    const build_ci_step = b.step("build-ci", "Build all binaries used by MiniCI");
    const build_roc_step = b.step("roc", "Build the roc compiler without running it");
    const run_roc_step = b.step("run-roc", "Build and run the roc cli");
    const build_check_tools_step = b.step("build-check-tools", "Build host check tools used by CI");
    const run_check_zig_format_step = b.step("run-check-zig-format", "Check formatting of all zig code");
    const run_check_zig_lints_step = b.step("run-check-zig-lints", "Run Zig lints");
    const run_check_tidy_step = b.step("run-check-tidy", "Run code tidiness checks");
    const run_check_git_lints_step = b.step("run-check-git-lints", "Run Git-backed code checks");
    const run_check_fx_platform_test_coverage_step = b.step("run-check-fx-platform-test-coverage", "Check that every .roc file in test/fx has a corresponding test");
    const run_check_type_checker_patterns_step = b.step("run-check-type-checker-patterns", "Check forbidden type-checker patterns");
    const run_check_enum_from_int_zero_step = b.step("run-check-enum-from-int-zero", "Check forbidden @enumFromInt(0) usage");
    const run_check_unused_suppression_step = b.step("run-check-unused-suppression", "Check unused-variable suppression patterns");
    const run_check_semantic_audit_step = b.step("run-check-semantic-audit", "Run the checked-data audit gate");
    const run_check_postcheck_architecture_step = b.step("run-check-postcheck-architecture", "Check that deleted post-check output/remapping APIs stay gone");
    const run_check_panic_step = b.step("run-check-panic", "Check forbidden panic usage in interpreter and builtins");
    const run_check_cli_global_stdio_step = b.step("run-check-cli-global-stdio", "Check forbidden global stdio usage in CLI code");
    const run_check_test_wiring_step = b.step("run-check-test-wiring", "Check test files are wired");
    const run_check_builtin_format_step = b.step("run-check-builtin-format", "Check Builtin.roc formatting");
    const build_snapshot_tool_step = b.step("build-snapshot-tool", "Build the snapshot tool");
    const run_check_snapshots_step = b.step("run-check-snapshots", "Regenerate snapshots and fail if tracked snapshots changed");
    const build_test_zig_step = b.step("build-test-zig", "Build Zig unit-test binaries");
    const run_test_zig_step = b.step("run-test-zig", "Run Zig unit tests");
    const build_test_lsp_integration_runner_step = b.step("build-test-lsp-integration-runner", "Build LSP integration test harness");
    const build_test_eval_runner_step = b.step("build-test-eval-runner", "Build eval test runner");
    const run_test_eval_step = b.step("run-test-eval", "Run eval tests in parallel across enabled backends");
    const build_test_eval_host_effects_runner_step = b.step("build-test-eval-host-effects-runner", "Build runtime host-effects eval test runner");
    const run_test_eval_host_effects_step = b.step("run-test-eval-host-effects", "Run runtime host-effects eval tests across supported backends");
    const build_playground_step = b.step("build-playground", "Build the WASM playground");
    const build_test_playground_runner_step = b.step("build-test-playground-runner", "Build the integration test suite for the WASM playground");
    const run_test_playground_step = b.step("run-test-playground", "Run the integration test suite for the WASM playground");
    const build_test_cli_runners_step = b.step("build-test-cli-runners", "Build CLI integration test runners");
    const run_test_cli_step = b.step("run-test-cli", "Run all CLI integration tests (platforms + subcommands + echo + glue)");
    const build_test_serialization_sizes_step = b.step("build-test-serialization-sizes", "Build serialization size checks");
    const run_test_serialization_sizes_step = b.step("run-test-serialization-sizes", "Verify Serialized types have platform-independent sizes");
    const build_test_wasm_static_lib_runner_step = b.step("build-test-wasm-static-lib-runner", "Build WASM static library test runner");
    const run_test_wasm_static_lib_step = b.step("run-test-wasm-static-lib", "Run WASM static library test runner");
    const run_test_dylib_step = b.step("run-test-dylib", "Build a Roc shared library and run it through the loader test");
    const build_coverage_tools_step = b.step("build-coverage-tools", "Build parser coverage tools");
    const run_coverage_parser_step = b.step("run-coverage-parser", "Run parser tests with kcov code coverage");
    const run_minici_step = b.step("minici", "Run a subset of CI build and test steps");
    const run_fmt_zig_step = b.step("run-fmt-zig", "Format all zig code");
    const run_snapshot_tool_step = b.step("run-snapshot-tool", "Run the snapshot tool to update snapshot files");
    const echo_wasm_step = b.step("build-echo-wasm", "Build the echo platform to zig-out/lib/echo.wasm");

    const build_test_hosts_step = b.step("build-test-hosts", "Build test platform host libraries");
    const build_release_step = b.step("build-release", "Build optimized release binary for distribution");

    // general configuration
    const target = blk: {
        var default_target_query: std.Target.Query = .{
            .abi = if (builtin.target.os.tag == .linux) .musl else null,
        };

        // Use x86_64_v3 (AVX2, no AVX-512) for Valgrind compatibility.
        // Valgrind 3.22 can't emulate AVX-512 EVEX instructions in musl startup code.
        // This matches the release target (getReleaseTargetQuery) which also uses x86_64_v3.
        if (builtin.target.cpu.arch == .x86_64) {
            default_target_query.cpu_model = .{ .explicit = &std.Target.x86.cpu.x86_64_v3 };
        }

        break :blk b.standardTargetOptions(.{ .default_target = default_target_query });
    };
    const optimize = b.standardOptimizeOption(.{});
    const strip_flag = b.option(bool, "strip", "Omit debug information");
    const no_bin = b.option(bool, "no-bin", "Skip emitting binaries (important for fast incremental compilation)") orelse false;
    const trace_eval = b.option(bool, "trace-eval", "Enable detailed evaluation tracing for debugging") orelse false;
    const trace_refcount = b.option(bool, "trace-refcount", "Enable detailed refcount tracing for debugging memory issues") orelse false;
    const trace_modules = b.option(bool, "trace-modules", "Enable module compilation and import resolution tracing") orelse false;
    const platform_filter = b.option([]const u8, "platform", "Filter which test platform to build (e.g., fx, str, int, fx-open)");
    const cli_test_llvm = b.option(bool, "cli-test-llvm", "Include LLVM size/speed backend jobs in CLI platform tests") orelse false;
    const trace_build = b.option(bool, "trace-build", "Enable detailed build pipeline tracing") orelse false;
    const debug_gpa = b.option(bool, "debug-gpa", "Use the leak-checking DebugAllocator for the roc binary even when libc is linked (default: off, so libc's malloc and its ASan/Valgrind/LD_PRELOAD tooling are used)") orelse false;
    const shared_memory_size = b.option(u64, "shared-memory-size", "Explicitly set shared-memory arena sizes in bytes");
    if (shared_memory_size) |size| {
        if (size == 0) {
            std.log.err("-Dshared-memory-size must be greater than 0", .{});
            std.process.exit(1);
        }
    }

    const parsed_args = parseBuildArgs(b);
    const run_args = parsed_args.run_args;
    const test_filters = parsed_args.test_filters;

    // llvm configuration
    // By default, use our bundled LLVM from roc-bootstrap. Users can opt-in to system LLVM
    // (e.g., for AFL++ fuzzing which requires system LLVM).
    const use_system_llvm = b.option(bool, "system-llvm", "Use system-installed LLVM instead of bundled LLVM (required for AFL++)") orelse false;
    const user_llvm_path = b.option([]const u8, "llvm-path", "Path to llvm. This path must contain the bin, lib, and include directory.");
    // Since zig afl is broken currently, default to system afl.
    const use_system_afl = b.option(bool, "system-afl", "Attempt to automatically detect and use system installed afl++") orelse true;

    if (user_llvm_path) |path| {
        // Even if the llvm backend is not enabled, still add the llvm path.
        // AFL++ may use it for building fuzzing executables.
        b.addSearchPrefix(b.pathJoin(&.{ path, "bin" }));
    }

    // tracy profiler configuration
    const flag_enable_tracy = b.option([]const u8, "tracy", "Enable Tracy integration. Supply path to Tracy source");
    const flag_tracy_callstack = b.option(bool, "tracy-callstack", "Include callstack information with Tracy data. Does nothing if -Dtracy is not provided") orelse false;
    const flag_tracy_allocation = b.option(bool, "tracy-allocation", "Include allocation information with Tracy data. Does nothing if -Dtracy is not provided") orelse (flag_enable_tracy != null);
    const flag_tracy_callstack_depth: u32 = b.option(u32, "tracy-callstack-depth", "Declare callstack depth for Tracy data. Does nothing if -Dtracy_callstack is not provided") orelse 10;
    if (flag_tracy_callstack) {
        std.log.warn("Tracy callstack is enable. This can significantly skew timings, but is important for understanding source location. Be cautious when generating timing and analyzing results.", .{});
    }

    // Create compile time build options
    const build_options = b.addOptions();
    build_options.addOption(bool, "enable_tracy", flag_enable_tracy != null);
    build_options.addOption(bool, "trace_eval", trace_eval);
    build_options.addOption(bool, "trace_refcount", trace_refcount);
    build_options.addOption(bool, "trace_modules", trace_modules);
    build_options.addOption(bool, "trace_build", trace_build);
    build_options.addOption(bool, "debug_gpa", debug_gpa);
    build_options.addOption(bool, "has_shared_memory_size", shared_memory_size != null);
    build_options.addOption(u64, "shared_memory_size", shared_memory_size orelse 0);
    const compiler_version_git = getCompilerVersionGit(b);
    build_options.addOption([]const u8, "compiler_version_git", compiler_version_git);
    build_options.addOption([32]u8, "compiler_artifact_hash", getCompilerArtifactHash(b, compiler_version_git));
    // `compiler_version` (e.g. "release-fast-abc12345") is assembled in the generated
    // build_options module so its build-mode prefix comes from @import("builtin").mode — the
    // actual optimization level of each compiled binary. The prefix can't be baked here because
    // build_options is shared between the dev `roc` exe (whose mode follows -Doptimize) and the
    // `release` exe (always built ReleaseFast); a single build-time value can't be right for both.
    build_options.contents.appendSlice(b.allocator,
        \\
        \\pub const compiler_version = @import("std").fmt.comptimePrint("{s}-{s}", .{
        \\    switch (@import("builtin").mode) {
        \\        .Debug => "debug",
        \\        .ReleaseSafe => "release-safe",
        \\        .ReleaseFast => "release-fast",
        \\        .ReleaseSmall => "release-small",
        \\    },
        \\    compiler_version_git,
        \\});
        \\
    ) catch @panic("OOM");
    build_options.addOption(bool, "enable_tracy_callstack", flag_tracy_callstack);
    build_options.addOption(bool, "enable_tracy_allocation", flag_tracy_allocation);
    build_options.addOption(u32, "tracy_callstack_depth", flag_tracy_callstack_depth);

    // Calculate effective strip value
    // - If strip is explicitly set by user, use that (warn if tracy_callstack is also set)
    // - Otherwise, default to stripping if not debug, unless tracy_callstack is enabled
    const strip: bool = blk: {
        if (strip_flag) |strip_bool| {
            // User explicitly set strip
            if (strip_bool and flag_tracy_callstack) {
                std.log.warn("Both -Dstrip and -Dtracy-callstack are enabled. " ++
                    "Stripping will remove callstack information needed by Tracy.", .{});
            }
            break :blk strip_bool;
        } else {
            // User did not set strip - use defaults
            if (flag_tracy_callstack) {
                // Don't strip when tracy callstack is enabled (preserves debug info)
                break :blk false;
            } else {
                // Default: strip in release modes
                break :blk optimize != .Debug;
            }
        }
    };

    // Don't omit frame pointer when tracy callstack is enabled (needed for callstack capture)
    const omit_frame_pointer: ?bool = if (flag_tracy_callstack) false else null;

    const target_is_native =
        // `query.isNative()` becomes false as soon as users override CPU features (e.g. -Dcpu=x86_64_v3),
        // but we still want to treat those builds as native so macOS can link against real FSEvents.
        target.result.os.tag == builtin.target.os.tag and
        target.result.cpu.arch == builtin.target.cpu.arch and
        target.result.abi == builtin.target.abi;
    build_options.addOption(bool, "target_is_native", target_is_native);

    // Path to bundled Darwin sysroot with libSystem.tbd stub
    const darwin_sysroot = b.path("src/cli/darwin").getPath(b);
    build_options.addOption([]const u8, "darwin_sysroot", darwin_sysroot);

    // We use zstd for `roc bundle` and `roc unbundle` and downloading .tar.zst bundles.
    const zstd = b.dependency("zstd", .{
        .target = target,
        .optimize = optimize,
    });

    const roc_modules = modules.RocModules.create(b, build_options, zstd);

    // Build-time compiler for builtin .roc modules
    //
    // Always rebuild builtins when building roc to ensure they match the compiler.
    // The builtin_compiler is cached by zig, so this only adds overhead when
    // compiler sources actually change.
    const builtin_roc_path = "src/build/roc/Builtin.roc";

    const write_compiled_builtins = b.addWriteFiles();

    // Always regenerate .bin files to ensure they match the current compiler
    const builtin_compiler = createAndRunBuiltinCompiler(b, roc_modules, flag_enable_tracy, &.{builtin_roc_path});
    write_compiled_builtins.step.dependOn(&builtin_compiler.run.step);

    // Copy tracked outputs from the builtin compiler run step.
    _ = write_compiled_builtins.addCopyFile(
        builtin_compiler.builtin_bin,
        "Builtin.bin",
    );

    // Copy the source Builtin.roc file for embedding
    _ = write_compiled_builtins.addCopyFile(
        b.path(builtin_roc_path),
        "Builtin.roc",
    );

    // Copy builtin_indices.bin
    _ = write_compiled_builtins.addCopyFile(
        builtin_compiler.builtin_indices_bin,
        "builtin_indices.bin",
    );

    // Generate compiled_builtins.zig with hardcoded Builtin module
    const builtins_source_str =
        \\pub const builtin_bin = @embedFile("Builtin.bin");
        \\pub const builtin_source = @embedFile("Builtin.roc");
        \\pub const builtin_indices_bin = @embedFile("builtin_indices.bin");
        \\
    ;

    const compiled_builtins_source = write_compiled_builtins.add(
        "compiled_builtins.zig",
        builtins_source_str,
    );

    const compiled_builtins_module = b.createModule(.{
        .root_source_file = compiled_builtins_source,
    });

    const bytebox = b.dependency("bytebox", .{
        .target = target,
        .optimize = optimize,
    });

    const zig_lints_exe = b.addExecutable(.{
        .name = "zig_lints",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ci/zig_lints.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });
    const tidy_exe = b.addExecutable(.{
        .name = "tidy",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ci/tidy.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });
    const test_wiring_exe = b.addExecutable(.{
        .name = "check_test_wiring",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ci/check_test_wiring.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });
    const minici_exe = b.addExecutable(.{
        .name = "minici",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/minici.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });

    const install_zig_lints = b.addInstallArtifact(zig_lints_exe, .{});
    const install_tidy = b.addInstallArtifact(tidy_exe, .{});
    const install_test_wiring = b.addInstallArtifact(test_wiring_exe, .{});
    const install_minici = b.addInstallArtifact(minici_exe, .{});
    build_check_tools_step.dependOn(&install_zig_lints.step);
    build_check_tools_step.dependOn(&install_tidy.step);
    build_check_tools_step.dependOn(&install_test_wiring.step);
    build_check_tools_step.dependOn(&install_minici.step);

    const run_zig_lints = b.addRunArtifact(zig_lints_exe);
    run_zig_lints.step.dependOn(&install_zig_lints.step);
    run_check_zig_lints_step.dependOn(&run_zig_lints.step);

    const run_tidy = b.addRunArtifact(tidy_exe);
    run_tidy.step.dependOn(&install_tidy.step);
    run_check_tidy_step.dependOn(&run_tidy.step);

    const run_git_lints = b.addRunArtifact(tidy_exe);
    run_git_lints.addArg("--git-lints");
    run_git_lints.step.dependOn(&install_tidy.step);
    run_check_git_lints_step.dependOn(&run_git_lints.step);

    const run_test_wiring = b.addRunArtifact(test_wiring_exe);
    run_test_wiring.step.dependOn(&install_test_wiring.step);
    run_check_test_wiring_step.dependOn(&run_test_wiring.step);

    const run_minici = b.addRunArtifact(minici_exe);
    run_minici.addArg(b.graph.zig_exe);
    run_minici.step.dependOn(&install_minici.step);
    run_minici_step.dependOn(&run_minici.step);

    roc_modules.compile.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.eval.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.eval.addImport("bytebox", bytebox.module("bytebox"));
    roc_modules.lsp.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.lsp_unit.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.lsp_integration.addImport("compiled_builtins", compiled_builtins_module);

    const check_test_env_module = b.createModule(.{
        .root_source_file = b.path("src/check/test_env_pkg.zig"),
    });
    check_test_env_module.addImport("tracy", roc_modules.tracy);
    check_test_env_module.addImport("builtins", roc_modules.builtins);
    check_test_env_module.addImport("collections", roc_modules.collections);
    check_test_env_module.addImport("base", roc_modules.base);
    check_test_env_module.addImport("parse", roc_modules.parse);
    check_test_env_module.addImport("types", roc_modules.types);
    check_test_env_module.addImport("can", roc_modules.can);
    check_test_env_module.addImport("reporting", roc_modules.reporting);
    check_test_env_module.addImport("compiled_builtins", compiled_builtins_module);

    // Build wasm32 builtins object at build time so the eval/REPL pipeline can
    // merge real compiled builtins into WASM modules (instead of using host imports).
    const wasm32_resolved_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none });
    const wasm32_builtins_obj = b.addObject(.{
        .name = "roc_builtins_wasm32_eval",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = wasm32_resolved_target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    wasm32_builtins_obj.root_module.addImport("tracy", b.addModule("tracy_stub_wasm32_eval", .{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    wasm32_builtins_obj.root_module.addImport("shim_io", b.addModule("shim_io_wasm32_eval", .{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    wasm32_builtins_obj.bundle_compiler_rt = false;
    configureBackend(wasm32_builtins_obj, wasm32_resolved_target);

    const wasm32_builtins_files = b.addWriteFiles();
    _ = wasm32_builtins_files.addCopyFile(wasm32_builtins_obj.getEmittedBin(), "roc_builtins.o");
    const wasm32_builtins_module = b.createModule(.{
        .root_source_file = wasm32_builtins_files.add("wasm32_builtins.zig",
            \\pub const bytes = @embedFile("roc_builtins.o");
            \\
        ),
    });
    roc_modules.eval.addImport("wasm32_builtins", wasm32_builtins_module);

    // Setup test platform host libraries
    const wasm_host_step = setupTestPlatforms(b, target, optimize, roc_modules, build_test_hosts_step, strip, omit_frame_pointer, platform_filter);
    const wasm_host_fixture_files = b.addWriteFiles();
    _ = wasm_host_fixture_files.addCopyFile(
        b.path("test/wasm/platform/targets/wasm32/host.wasm"),
        "host.wasm",
    );
    const wasm_host_fixture_module = b.createModule(.{
        .root_source_file = wasm_host_fixture_files.add(
            "wasm_host_fixture.zig",
            "pub const host_wasm = @embedFile(\"host.wasm\");\n",
        ),
    });
    wasm_host_fixture_files.step.dependOn(wasm_host_step);

    const llvm_codegen_module = b.addModule("llvm_codegen", .{
        .root_source_file = b.path("src/backend/llvm/MonoLlvmCodeGen.zig"),
    });
    llvm_codegen_module.addImport("layout", roc_modules.layout);
    llvm_codegen_module.addImport("lir", roc_modules.lir);
    llvm_codegen_module.addImport("ctx", roc_modules.ctx);
    llvm_codegen_module.addImport("builtins", roc_modules.builtins);

    const roc_exe = addMainExe(b, roc_modules, target, optimize, strip, omit_frame_pointer, use_system_llvm, user_llvm_path, flag_enable_tracy, zstd, compiled_builtins_module, write_compiled_builtins, llvm_codegen_module, flag_enable_tracy) orelse return;
    roc_modules.addAll(roc_exe);
    _ = install_and_run(b, no_bin, roc_exe, build_roc_step, run_roc_step, run_args);

    // Clear the Roc cache when building the compiler to ensure stale cached artifacts aren't used
    const clear_cache_step = createClearCacheStep(b);
    build_roc_step.dependOn(clear_cache_step);
    b.getInstallStep().dependOn(clear_cache_step);

    const run_builtin_format = b.addRunArtifact(roc_exe);
    run_builtin_format.addArgs(&.{ "fmt", "--check", "src/build/roc/Builtin.roc" });
    run_builtin_format.step.dependOn(build_roc_step);
    run_check_builtin_format_step.dependOn(&run_builtin_format.step);

    // Release build with platform-optimal settings
    {
        const release_target = b.resolveTargetQuery(getReleaseTargetQuery());
        // Create a release-specific zstd dependency with release settings
        const release_zstd = b.dependency("zstd", .{
            .target = release_target,
            .optimize = .ReleaseFast,
        });
        const release_exe = addMainExe(
            b,
            roc_modules,
            release_target,
            .ReleaseFast, // Always ReleaseFast for release
            true, // Always strip for release
            null, // Default frame pointer handling
            use_system_llvm,
            user_llvm_path,
            null, // No tracy for release
            release_zstd,
            compiled_builtins_module,
            write_compiled_builtins,
            llvm_codegen_module,
            null, // No tracy
        );
        if (release_exe) |exe| {
            roc_modules.addAll(exe);
            exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
            exe.step.dependOn(&write_compiled_builtins.step);
            const install = b.addInstallArtifact(exe, .{});
            build_release_step.dependOn(&install.step);
        }
    }

    // Unified test platform runner (replaces fx_cross_runner and int_cross_runner)
    const test_runner_exe = b.addExecutable(.{
        .name = "test_runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/test/test_runner.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{},
            // runner_core.zig uses std.c.{timespec,clock_gettime,environ}; Zig 0.16 requires
            // explicit libc linkage for any module that touches std.c.
            .link_libc = true,
        }),
    });
    b.installArtifact(test_runner_exe);

    // Store CLI runner step reference so we can add glue host dependency later.
    var run_cli_test_step: ?*std.Build.Step = null;

    // CLI integration tests: one harness-backed runner covers platforms,
    // subcommands, echo, and glue. Focus locally with:
    //   zig build run-test-cli -- --suite echo --filter "case name"
    if (!no_bin) {
        const install = b.addInstallArtifact(roc_exe, .{});

        const parallel_cli_runner_exe = b.addExecutable(.{
            .name = "parallel_cli_runner",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/test/parallel_cli_runner.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "test_harness", .module = b.createModule(.{
                        .root_source_file = b.path("src/build/test_harness.zig"),
                        .imports = &.{.{ .name = "collections", .module = roc_modules.collections }},
                    }) },
                    .{ .name = "collections", .module = roc_modules.collections },
                },
            }),
        });
        parallel_cli_runner_exe.root_module.link_libc = true;
        build_test_cli_runners_step.dependOn(&parallel_cli_runner_exe.step);

        const run_cli = b.addRunArtifact(parallel_cli_runner_exe);
        run_cli.addArg("zig-out/bin/roc");
        if (cli_test_llvm) {
            run_cli.addArg("--include-llvm");
        }
        for (test_filters) |f| {
            run_cli.addArg("--filter");
            run_cli.addArg(f);
        }
        if (run_args.len != 0) {
            run_cli.addArgs(run_args);
        }
        run_cli.step.dependOn(&install.step);
        run_cli.step.dependOn(build_test_hosts_step);
        run_cli_test_step = &run_cli.step;
        run_test_cli_step.dependOn(&run_cli.step);
    }

    // Manual rebuild command: zig build run-rebuild-builtins
    // Use this after making compiler changes to ensure those changes are reflected in builtins
    const rebuild_builtins_step = b.step(
        "run-rebuild-builtins",
        "Force rebuild of all builtin modules (*.roc -> *.bin)",
    );

    // Clean zig-out/ to ensure a fresh rebuild of builtins
    // Note: We don't delete .zig-cache because it contains build options needed during compilation.
    const clean_out_step = RemoveDirTreeStep.create(b, "zig-out");

    // Also clear the roc cache to avoid stale cached modules with old struct layouts
    const clear_roc_cache_step = createClearCacheStep(b);

    // Discover .roc files again for the rebuild command
    const roc_files_force = discoverBuiltinRocFiles(b) catch |err| {
        std.debug.print("Failed to discover .roc files for rebuild: {}\n", .{err});
        return;
    };

    const run_builtin_compiler_force = createAndRunBuiltinCompiler(b, roc_modules, flag_enable_tracy, roc_files_force);
    run_builtin_compiler_force.run.step.dependOn(&clean_out_step.step);
    run_builtin_compiler_force.run.step.dependOn(clear_roc_cache_step);
    rebuild_builtins_step.dependOn(&run_builtin_compiler_force.run.step);

    // Add the compiled builtins module to roc exe and make it depend on the builtins being ready
    roc_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    roc_exe.step.dependOn(&write_compiled_builtins.step);

    roc_modules.eval.addAnonymousImport("llvm_compile", .{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
        .imports = &.{
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "layout", .module = roc_modules.layout },
            .{ .name = "backend", .module = roc_modules.backend },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "llvm_codegen", .module = llvm_codegen_module },
            .{ .name = "build_options", .module = roc_modules.build_options },
            .{ .name = "embedded_lld", .module = roc_modules.embedded_lld },
        },
    });

    const builtins_bc_obj = b.addObject(.{
        .name = "roc_builtins_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = target,
            .optimize = .ReleaseFast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins_bc_obj.root_module.addImport("tracy", b.addModule("tracy_stub_bc", .{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins_bc_obj.root_module.addImport("shim_io", b.addModule("shim_io_bc", .{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins_bc_obj.root_module.omit_frame_pointer = true;
    builtins_bc_obj.root_module.stack_check = false;
    builtins_bc_obj.root_module.link_libc = true;
    builtins_bc_obj.use_llvm = true;
    builtins_bc_obj.bundle_compiler_rt = true;
    _ = builtins_bc_obj.getEmittedBin();
    const builtins_bc_file = builtins_bc_obj.getEmittedLlvmBc();

    const llvm_embedded_files = b.addWriteFiles();
    _ = llvm_embedded_files.addCopyFile(builtins_bc_file, "builtins.bc");

    const llvm_embedded_source: []const u8 =
        \\pub const builtins_bc = @embedFile("builtins.bc");
        \\
    ;

    const llvm_embedded_module = b.createModule(.{
        .root_source_file = llvm_embedded_files.add("llvm_embedded.zig", llvm_embedded_source),
    });
    roc_exe.step.dependOn(&llvm_embedded_files.step);

    roc_modules.eval.addAnonymousImport("llvm_compile", .{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
        .imports = &.{
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "layout", .module = roc_modules.layout },
            .{ .name = "backend", .module = roc_modules.backend },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "llvm_codegen", .module = llvm_codegen_module },
            .{ .name = "build_options", .module = roc_modules.build_options },
            .{ .name = "llvm_embedded", .module = llvm_embedded_module },
            .{ .name = "embedded_lld", .module = roc_modules.embedded_lld },
        },
    });

    // Add snapshot tool
    const snapshot_exe = b.addExecutable(.{
        .name = "snapshot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/snapshot_tool/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(snapshot_exe, target);
    roc_modules.addAll(snapshot_exe);
    snapshot_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    snapshot_exe.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        snapshot_exe,
        target,
        use_system_llvm,
        user_llvm_path,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (snapshot_exe.root_module.resolved_target.?.result.os.tag != .windows or
        snapshot_exe.root_module.resolved_target.?.result.abi != .msvc)
    {
        snapshot_exe.root_module.link_libcpp = true;
    }

    add_tracy(b, roc_modules.build_options, snapshot_exe, target, true, flag_enable_tracy);
    const snapshot_exe_install = install_and_run(
        b,
        no_bin,
        snapshot_exe,
        build_snapshot_tool_step,
        run_snapshot_tool_step,
        run_args,
    );
    const check_snapshot_diff = b.addSystemCommand(&.{ "git", "diff", "--exit-code", "test/snapshots" });
    check_snapshot_diff.step.dependOn(run_snapshot_tool_step);
    run_check_snapshots_step.dependOn(&check_snapshot_diff.step);

    // Add parallel eval test runner
    const eval_test_exe = b.addExecutable(.{
        .name = "eval-test-runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/parallel_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true, // needed for sljmp/setjmp
        }),
    });
    // The deepest eval test recurses ~1000 frames; Zig 0.16 codegen pushes that past
    // the 1 MiB Windows default. Reserve a generous stack so recursive eval tests
    // don't trip our SetUnhandledExceptionFilter stack-overflow handler.
    eval_test_exe.stack_size = 64 * 1024 * 1024;
    configureBackend(eval_test_exe, target);
    roc_modules.addAll(eval_test_exe);
    eval_test_exe.root_module.addOptions("coverage_options", blk: {
        const opts = b.addOptions();
        opts.addOption(bool, "coverage", false);
        break :blk opts;
    });
    eval_test_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    eval_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
    eval_test_exe.root_module.addImport("test_harness", b.createModule(.{
        .root_source_file = b.path("src/build/test_harness.zig"),
        .imports = &.{.{ .name = "collections", .module = roc_modules.collections }},
    }));
    eval_test_exe.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        eval_test_exe,
        target,
        use_system_llvm,
        user_llvm_path,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (eval_test_exe.root_module.resolved_target.?.result.os.tag != .windows or
        eval_test_exe.root_module.resolved_target.?.result.abi != .msvc)
    {
        eval_test_exe.root_module.link_libcpp = true;
    }
    // Build eval runner args: forward all --test-filter values as --filter args.
    const eval_run_args = if (test_filters.len > 0) blk: {
        var eval_args_list = std.ArrayList([]const u8).empty;
        for (run_args) |arg| {
            eval_args_list.append(b.allocator, arg) catch @panic("OOM");
        }
        for (test_filters) |f| {
            eval_args_list.append(b.allocator, "--filter") catch @panic("OOM");
            eval_args_list.append(b.allocator, f) catch @panic("OOM");
        }
        break :blk eval_args_list.toOwnedSlice(b.allocator) catch @panic("OOM");
    } else run_args;
    _ = install_and_run(
        b,
        no_bin,
        eval_test_exe,
        build_test_eval_runner_step,
        run_test_eval_step,
        eval_run_args,
    );

    const eval_host_effects_exe = b.addExecutable(.{
        .name = "eval-host-effects-runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/host_effects_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(eval_host_effects_exe, target);
    roc_modules.addAll(eval_host_effects_exe);
    eval_host_effects_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    eval_host_effects_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
    eval_host_effects_exe.root_module.addImport("test_harness", b.createModule(.{
        .root_source_file = b.path("src/build/test_harness.zig"),
        .imports = &.{.{ .name = "collections", .module = roc_modules.collections }},
    }));
    eval_host_effects_exe.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        eval_host_effects_exe,
        target,
        use_system_llvm,
        user_llvm_path,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (eval_host_effects_exe.root_module.resolved_target.?.result.os.tag != .windows or
        eval_host_effects_exe.root_module.resolved_target.?.result.abi != .msvc)
    {
        eval_host_effects_exe.root_module.link_libcpp = true;
    }
    const eval_host_effects_run_args = if (test_filters.len > 0) blk: {
        var eval_args_list = std.ArrayList([]const u8).empty;
        for (run_args) |arg| {
            eval_args_list.append(b.allocator, arg) catch @panic("OOM");
        }
        for (test_filters) |f| {
            eval_args_list.append(b.allocator, "--filter") catch @panic("OOM");
            eval_args_list.append(b.allocator, f) catch @panic("OOM");
        }
        break :blk eval_args_list.toOwnedSlice(b.allocator) catch @panic("OOM");
    } else run_args;
    _ = install_and_run(
        b,
        no_bin,
        eval_host_effects_exe,
        build_test_eval_host_effects_runner_step,
        run_test_eval_host_effects_step,
        eval_host_effects_run_args,
    );

    const playground_exe = b.addExecutable(.{
        .name = "playground",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/playground_wasm/main.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = optimize,
        }),
    });
    configureBackend(playground_exe, b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    }));
    playground_exe.entry = .disabled;
    playground_exe.rdynamic = true;
    playground_exe.link_function_sections = true;
    playground_exe.import_memory = false;
    roc_modules.addAll(playground_exe);
    playground_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    playground_exe.step.dependOn(&write_compiled_builtins.step);

    add_tracy(b, roc_modules.build_options, playground_exe, b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    }), false, null);

    const playground_install = b.addInstallArtifact(playground_exe, .{});
    build_playground_step.dependOn(&playground_install.step);

    // Build echo.wasm — echo platform compiled to wasm32-freestanding.
    // Also serves as a regression test that the compile module stays wasm-compatible.
    {
        const echo_wasm_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding });
        const echo_wasm = b.addExecutable(.{
            .name = "echo",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/echo_platform/echo.zig"),
                .target = echo_wasm_target,
                .optimize = .ReleaseSmall,
            }),
        });
        configureBackend(echo_wasm, echo_wasm_target);
        echo_wasm.entry = .disabled;
        echo_wasm.rdynamic = true;
        echo_wasm.root_module.addImport("compile", roc_modules.compile);
        echo_wasm.root_module.addImport("check", roc_modules.check);
        echo_wasm.root_module.addImport("eval", roc_modules.eval);
        echo_wasm.root_module.addImport("lir", roc_modules.lir);
        echo_wasm.root_module.addImport("layout", roc_modules.layout);
        echo_wasm.root_module.addImport("base", roc_modules.base);
        echo_wasm.root_module.addImport("can", roc_modules.can);
        echo_wasm.root_module.addImport("echo_platform", roc_modules.echo_platform);
        echo_wasm.root_module.addImport("reporting", roc_modules.reporting);
        echo_wasm.root_module.addImport("roc_target", roc_modules.roc_target);
        echo_wasm.root_module.addImport("compiled_builtins", compiled_builtins_module);
        echo_wasm.root_module.addImport("WasmFilesystem.zig", b.createModule(.{
            .root_source_file = b.path("src/playground_wasm/WasmFilesystem.zig"),
            .target = echo_wasm_target,
            .imports = &.{.{ .name = "ctx", .module = roc_modules.ctx }},
        }));
        echo_wasm.step.dependOn(&write_compiled_builtins.step);

        const echo_wasm_install = b.addInstallArtifact(echo_wasm, .{
            .dest_dir = .{ .override = .lib },
        });
        build_playground_step.dependOn(&echo_wasm_install.step);
        echo_wasm_step.dependOn(&echo_wasm_install.step);

        // Copy the echo platform www files alongside echo.wasm
        inline for (.{ "index.html", "app.js" }) |filename| {
            const install_file = b.addInstallFile(b.path("src/echo_platform/www/" ++ filename), "lib/" ++ filename);
            build_playground_step.dependOn(&install_file.step);
            echo_wasm_step.dependOn(&install_file.step);
        }

        // echo_native: native binary that drives the same runEcho pipeline as
        // echo.wasm. Use it to debug compile/run failures with real stack
        // traces. `zig build run-echo -- path/to/app.roc [--with-file Name=path] ...`
        const echo_native_exe = b.addExecutable(.{
            .name = "echo_native",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/echo_platform/echo_native.zig"),
                .target = target,
                .optimize = optimize,
                // CoreCtx.default's OS vtable pulls in std.c.getenv (and
                // transitively std.net's getaddrinfo); Zig 0.16 requires
                // libc to be linked explicitly when std.c.* is referenced.
                .link_libc = true,
            }),
        });
        configureBackend(echo_native_exe, target);
        echo_native_exe.root_module.addImport("compile", roc_modules.compile);
        echo_native_exe.root_module.addImport("check", roc_modules.check);
        echo_native_exe.root_module.addImport("eval", roc_modules.eval);
        echo_native_exe.root_module.addImport("lir", roc_modules.lir);
        echo_native_exe.root_module.addImport("layout", roc_modules.layout);
        echo_native_exe.root_module.addImport("base", roc_modules.base);
        echo_native_exe.root_module.addImport("can", roc_modules.can);
        echo_native_exe.root_module.addImport("echo_platform", roc_modules.echo_platform);
        echo_native_exe.root_module.addImport("reporting", roc_modules.reporting);
        echo_native_exe.root_module.addImport("roc_target", roc_modules.roc_target);
        echo_native_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
        echo_native_exe.step.dependOn(&write_compiled_builtins.step);

        const echo_native_install = b.addInstallArtifact(echo_native_exe, .{});

        const run_echo_step = b.step("run-echo", "Run the native echo platform driver (debug helper for echo.wasm)");
        const run_echo_cmd = b.addRunArtifact(echo_native_exe);
        if (run_args.len != 0) run_echo_cmd.addArgs(run_args);
        run_echo_cmd.step.dependOn(&echo_native_install.step);
        run_echo_step.dependOn(&run_echo_cmd.step);

        // test-echo-wasm: bytebox-driven integration test that loads
        // zig-out/lib/echo.wasm, supplies in-process js_echo + js_stderr,
        // and asserts the tutorial example produces the expected output.
        const echo_wasm_test_exe = b.addExecutable(.{
            .name = "echo_wasm_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/echo-wasm-test/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(echo_wasm_test_exe, target);
        echo_wasm_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));

        const run_test_echo_wasm_step = b.step("run-test-echo-wasm", "Run echo.wasm tutorial example through bytebox");
        const run_echo_wasm_test = b.addRunArtifact(echo_wasm_test_exe);
        // Ensure the wasm is built before the test runs.
        run_echo_wasm_test.step.dependOn(&echo_wasm_install.step);
        run_test_echo_wasm_step.dependOn(&run_echo_wasm_test.step);
    }

    // Build playground integration tests - now enabled for all optimization modes.
    const playground_test_install = blk: {
        const playground_test_optimize: std.builtin.OptimizeMode = if (optimize == .Debug) .ReleaseSafe else optimize;
        const playground_wasm_target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        });
        const playground_test_wasm = b.addExecutable(.{
            .name = "playground-test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/playground_wasm/main.zig"),
                .target = playground_wasm_target,
                .optimize = playground_test_optimize,
            }),
        });
        configureBackend(playground_test_wasm, playground_wasm_target);
        playground_test_wasm.entry = .disabled;
        playground_test_wasm.rdynamic = true;
        playground_test_wasm.link_function_sections = true;
        playground_test_wasm.import_memory = false;
        roc_modules.addAll(playground_test_wasm);
        playground_test_wasm.root_module.addImport("compiled_builtins", compiled_builtins_module);
        playground_test_wasm.step.dependOn(&write_compiled_builtins.step);
        add_tracy(b, roc_modules.build_options, playground_test_wasm, playground_wasm_target, false, null);

        const playground_integration_test_exe = b.addExecutable(.{
            .name = "playground_integration_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/playground-integration/main.zig"),
                .target = target,
                .optimize = playground_test_optimize,
            }),
        });
        configureBackend(playground_integration_test_exe, target);
        playground_integration_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
        playground_integration_test_exe.root_module.addImport("build_options", build_options.createModule());
        playground_integration_test_exe.root_module.addImport("test_harness", b.createModule(.{
            .root_source_file = b.path("src/build/test_harness.zig"),
        }));
        roc_modules.addAll(playground_integration_test_exe);

        const install = b.addInstallArtifact(playground_integration_test_exe, .{});
        install.step.dependOn(&playground_test_wasm.step);
        build_test_playground_runner_step.dependOn(&install.step);

        const run_playground_test = b.addRunArtifact(playground_integration_test_exe);
        run_playground_test.addArg("--wasm-path");
        run_playground_test.addFileArg(playground_test_wasm.getEmittedBin());
        for (test_filters) |f| {
            run_playground_test.addArg("--filter");
            run_playground_test.addArg(f);
        }
        if (run_args.len != 0) {
            run_playground_test.addArgs(run_args);
        }
        run_playground_test.step.dependOn(&install.step);
        run_test_playground_step.dependOn(&run_playground_test.step);

        break :blk install;
    };

    // Add serialization size check
    // This verifies that Serialized types have the same size on 32-bit and 64-bit platforms
    // using compile-time assertions
    {
        // Build for native - will fail at compile time if sizes don't match expected
        const size_check_native = b.addExecutable(.{
            .name = "serialization_size_check_native",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/serialization_size_check.zig"),
                .target = target,
                .optimize = .Debug,
            }),
        });
        configureBackend(size_check_native, target);
        roc_modules.addAll(size_check_native);

        // Build for wasm32 (32-bit) - will fail at compile time if sizes don't match expected
        const size_check_wasm32 = b.addExecutable(.{
            .name = "serialization_size_check_wasm32",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/serialization_size_check.zig"),
                .target = b.resolveTargetQuery(.{
                    .cpu_arch = .wasm32,
                    .os_tag = .freestanding,
                }),
                .optimize = .Debug,
            }),
        });
        configureBackend(size_check_wasm32, b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }));
        size_check_wasm32.entry = .disabled;
        size_check_wasm32.rdynamic = true;
        roc_modules.addAll(size_check_wasm32);

        // Run the native version to confirm (wasm32 build is enough to verify 32-bit)
        const run_native = b.addRunArtifact(size_check_native);

        // The test passes if both executables build successfully (compile-time checks pass)
        // and the native one runs without error
        build_test_serialization_sizes_step.dependOn(&size_check_native.step);
        build_test_serialization_sizes_step.dependOn(&size_check_wasm32.step);
        run_test_serialization_sizes_step.dependOn(build_test_serialization_sizes_step);
        run_test_serialization_sizes_step.dependOn(&run_native.step);
    }

    // Build WASM static library fixture and test runner with bytebox.
    {
        const build_wasm_app = b.addRunArtifact(roc_exe);
        build_wasm_app.addArgs(&.{
            "build",
            "test/wasm/app.roc",
            "--target=wasm32",
            "--output=test/wasm/app.wasm",
        });
        build_wasm_app.step.dependOn(build_test_hosts_step);
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_app.step);

        const build_wasm_rc_cleanup_app = b.addRunArtifact(roc_exe);
        build_wasm_rc_cleanup_app.addArgs(&.{
            "build",
            "test/wasm/rc_cleanup_static_lib_app.roc",
            "--opt=dev",
            "--target=wasm32",
            "--output=test/wasm/rc_cleanup_static_lib_app.wasm",
        });
        build_wasm_rc_cleanup_app.step.dependOn(build_test_hosts_step);
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_rc_cleanup_app.step);

        const build_wasm_rc_cleanup_model_list_app = b.addRunArtifact(roc_exe);
        build_wasm_rc_cleanup_model_list_app.addArgs(&.{
            "build",
            "test/wasm/rc_cleanup_model_list_static_lib_app.roc",
            "--opt=dev",
            "--target=wasm32",
            "--output=test/wasm/rc_cleanup_model_list_static_lib_app.wasm",
        });
        build_wasm_rc_cleanup_model_list_app.step.dependOn(build_test_hosts_step);
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_rc_cleanup_model_list_app.step);

        const wasm_test_exe = b.addExecutable(.{
            .name = "wasm_static_lib_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/wasm/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(wasm_test_exe, target);
        wasm_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));

        const install = b.addInstallArtifact(wasm_test_exe, .{});
        build_test_wasm_static_lib_runner_step.dependOn(&install.step);

        const run_wasm_test = b.addRunArtifact(wasm_test_exe);
        if (run_args.len != 0) {
            run_wasm_test.addArgs(run_args);
        } else {
            const run_wasm_rc_cleanup_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_rc_cleanup_test.addArgs(&.{
                "--wasm-path",
                "test/wasm/rc_cleanup_static_lib_app.wasm",
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                "--min-allocs",
                "16",
            });
            run_wasm_rc_cleanup_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_rc_cleanup_test.step);

            const run_wasm_rc_cleanup_model_list_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_rc_cleanup_model_list_test.addArgs(&.{
                "--wasm-path",
                "test/wasm/rc_cleanup_model_list_static_lib_app.wasm",
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                "--min-allocs",
                "64",
            });
            run_wasm_rc_cleanup_model_list_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_rc_cleanup_model_list_test.step);
        }
        run_wasm_test.step.dependOn(build_test_wasm_static_lib_runner_step);
        run_test_wasm_static_lib_step.dependOn(&run_wasm_test.step);
    }

    // Build the shared-library test fixture with `roc build` and verify it by
    // running a separate loader executable that dlopens it and calls its C API.
    {
        const dylib_ext = switch (target.result.os.tag) {
            .windows => ".dll",
            .macos => ".dylib",
            else => ".so",
        };
        const dylib_output = b.fmt("test/dylib/app{s}", .{dylib_ext});

        const build_dylib_app = b.addRunArtifact(roc_exe);
        build_dylib_app.addArgs(&.{
            "build",
            "test/dylib/app.roc",
            "--opt=size",
            b.fmt("--output={s}", .{dylib_output}),
        });
        build_dylib_app.step.dependOn(build_test_hosts_step);

        const dylib_loader_exe = b.addExecutable(.{
            .name = "dylib_loader",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/dylib/loader.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(dylib_loader_exe, target);

        const install_dylib_loader = b.addInstallArtifact(dylib_loader_exe, .{});

        const run_dylib_test = b.addRunArtifact(dylib_loader_exe);
        run_dylib_test.step.dependOn(&install_dylib_loader.step);
        run_dylib_test.addArg(dylib_output);
        run_dylib_test.step.dependOn(&build_dylib_app.step);
        run_test_dylib_step.dependOn(&run_dylib_test.step);
    }

    // Check fx platform test coverage convenience step
    const checkfx_inner = CheckFxStep.create(b);
    run_check_fx_platform_test_coverage_step.dependOn(&checkfx_inner.step);

    const stack_overflow_test_helper_exe = b.addExecutable(.{
        .name = "stack_overflow_test_helper",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/stack_overflow_test_helper.zig"),
            .target = target,
            .optimize = optimize,
            // stack_overflow.zig uses std.c.write/fork/pipe; Zig 0.16 requires explicit link_libc.
            .link_libc = true,
        }),
    });
    stack_overflow_test_helper_exe.root_module.addImport("base", roc_modules.base);
    roc_modules.addModuleDependencies(stack_overflow_test_helper_exe, .base);
    const install_stack_overflow_test_helper = b.addInstallArtifact(stack_overflow_test_helper_exe, .{});
    const stack_overflow_test_helper_path = b.getInstallPath(.bin, stack_overflow_test_helper_exe.out_filename);
    build_test_zig_step.dependOn(&install_stack_overflow_test_helper.step);

    // Create and add module tests
    const module_tests_result = roc_modules.createModuleTests(b, target, optimize, zstd, test_filters);
    const tests_summary = TestsSummaryStep.create(b, test_filters, module_tests_result.forced_passes);
    if (builtin.os.tag == .windows) {
        // Zig 0.16's Windows test runner IPC can time out while many Roc test
        // binaries are starting at once. Keep the same tests, but start them
        // in a deterministic order.
        tests_summary.setRunSerialization();
    }

    for (module_tests_result.tests) |module_test| {
        // Add compiled builtins to tests that canonicalize ordinary modules.
        if (std.mem.eql(u8, module_test.test_step.name, "can") or std.mem.eql(u8, module_test.test_step.name, "check") or std.mem.eql(u8, module_test.test_step.name, "eval") or std.mem.eql(u8, module_test.test_step.name, "compile") or std.mem.eql(u8, module_test.test_step.name, "lsp_unit") or std.mem.eql(u8, module_test.test_step.name, "lsp_integration")) {
            module_test.test_step.root_module.addImport("compiled_builtins", compiled_builtins_module);
            module_test.test_step.step.dependOn(&write_compiled_builtins.step);
        }

        if (std.mem.eql(u8, module_test.test_step.name, "repl")) {
            module_test.test_step.root_module.addImport("bytebox", bytebox.module("bytebox"));
        }

        // Add bytebox and wasm32 builtins to eval tests for wasm backend testing
        if (std.mem.eql(u8, module_test.test_step.name, "eval")) {
            module_test.test_step.root_module.addImport("bytebox", bytebox.module("bytebox"));
            module_test.test_step.root_module.addImport("wasm32_builtins", wasm32_builtins_module);
            const compile_build_module = b.createModule(.{
                .root_source_file = b.path("src/compile/compile_build.zig"),
            });
            compile_build_module.addImport("tracy", roc_modules.tracy);
            compile_build_module.addImport("build_options", roc_modules.build_options);
            compile_build_module.addImport("ctx", roc_modules.ctx);
            compile_build_module.addImport("builtins", roc_modules.builtins);
            compile_build_module.addImport("collections", roc_modules.collections);
            compile_build_module.addImport("base", roc_modules.base);
            compile_build_module.addImport("types", roc_modules.types);
            compile_build_module.addImport("parse", roc_modules.parse);
            compile_build_module.addImport("can", roc_modules.can);
            compile_build_module.addImport("check", roc_modules.check);
            compile_build_module.addImport("reporting", roc_modules.reporting);
            compile_build_module.addImport("layout", roc_modules.layout);
            compile_build_module.addImport("eval", module_test.test_step.root_module);
            compile_build_module.addImport("unbundle", roc_modules.unbundle);
            compile_build_module.addImport("roc_target", roc_modules.roc_target);
            compile_build_module.addImport("compiled_builtins", compiled_builtins_module);
            module_test.test_step.root_module.addImport("compile_build", compile_build_module);
            try addLlvmSupportToStep(
                b,
                module_test.test_step,
                target,
                use_system_llvm,
                user_llvm_path,
                roc_modules,
                llvm_codegen_module,
                llvm_embedded_module,
                zstd,
            );
        }

        // Backend tests need the wasm host object and builtins for WASM linking tests
        if (std.mem.eql(u8, module_test.test_step.name, "backend")) {
            module_test.test_step.step.dependOn(wasm_host_step);
            module_test.test_step.step.dependOn(&wasm_host_fixture_files.step);
            module_test.test_step.root_module.addImport("wasm_host_fixture", wasm_host_fixture_module);
            module_test.test_step.root_module.addImport("wasm32_builtins", wasm32_builtins_module);
            module_test.test_step.root_module.addImport("bytebox", bytebox.module("bytebox"));
        }

        if (std.mem.eql(u8, module_test.test_step.name, "repl")) {
            try addLlvmSupportToStep(
                b,
                module_test.test_step,
                target,
                use_system_llvm,
                user_llvm_path,
                roc_modules,
                llvm_codegen_module,
                llvm_embedded_module,
                zstd,
            );
        }

        if (run_args.len != 0) {
            module_test.run_step.addArgs(run_args);
        }
        if (std.mem.eql(u8, module_test.test_step.name, "base")) {
            module_test.run_step.step.dependOn(&install_stack_overflow_test_helper.step);
            module_test.run_step.setEnvironmentVariable("ROC_STACK_OVERFLOW_TEST_HELPER", stack_overflow_test_helper_path);
        }

        // Create individual test step for this module
        const test_exe_name = module_test.test_step.name;
        const run_module_test_step = b.step(
            b.fmt("run-test-zig-module-{s}", .{test_exe_name}),
            b.fmt("Run {s} Zig module tests", .{test_exe_name}),
        );

        // Create run step that accepts command line args (including --test-filter)
        const individual_run = b.addRunArtifact(module_test.test_step);
        if (run_args.len != 0) {
            individual_run.addArgs(run_args);
        }
        if (std.mem.eql(u8, module_test.test_step.name, "base")) {
            individual_run.step.dependOn(&install_stack_overflow_test_helper.step);
            individual_run.setEnvironmentVariable("ROC_STACK_OVERFLOW_TEST_HELPER", stack_overflow_test_helper_path);
        }
        run_module_test_step.dependOn(&individual_run.step);

        b.default_step.dependOn(&module_test.test_step.step);
        build_test_zig_step.dependOn(&module_test.test_step.step);
        tests_summary.addRun(&module_test.run_step.step);
    }

    const lsp_integration_test_harness_module = b.createModule(.{
        .root_source_file = b.path("src/build/test_harness.zig"),
    });
    const lsp_integration_runner_exe = b.addExecutable(.{
        .name = "parallel_lsp_integration_runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/lsp/test/parallel_integration_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "test_harness", .module = lsp_integration_test_harness_module },
                .{ .name = "integration_specs", .module = roc_modules.lsp_integration },
            },
        }),
    });
    add_tracy(b, roc_modules.build_options, lsp_integration_runner_exe, target, false, flag_enable_tracy);
    lsp_integration_runner_exe.step.dependOn(&write_compiled_builtins.step);
    build_test_lsp_integration_runner_step.dependOn(&lsp_integration_runner_exe.step);

    const run_lsp_integration = b.addRunArtifact(lsp_integration_runner_exe);
    run_lsp_integration.expectExitCode(0);
    for (test_filters) |filter| {
        run_lsp_integration.addArg("--filter");
        run_lsp_integration.addArg(filter);
    }
    if (run_args.len != 0) {
        run_lsp_integration.addArgs(run_args);
    }

    const run_lsp_integration_step = b.step(
        "run-test-zig-module-lsp_integration",
        "Run LSP integration tests in parallel",
    );
    run_lsp_integration_step.dependOn(&run_lsp_integration.step);

    b.default_step.dependOn(&lsp_integration_runner_exe.step);
    build_test_zig_step.dependOn(&lsp_integration_runner_exe.step);
    run_test_zig_step.dependOn(&run_lsp_integration.step);

    // Add snapshot tool test
    const enable_snapshot_tests = b.option(bool, "snapshot-tests", "Enable snapshot tests") orelse true;
    if (enable_snapshot_tests) {
        const snapshot_test = b.addTest(.{
            .name = "snapshot_tool_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/snapshot_tool/main.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(snapshot_test);
        snapshot_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        snapshot_test.step.dependOn(&write_compiled_builtins.step);
        try addLlvmSupportToStep(
            b,
            snapshot_test,
            target,
            use_system_llvm,
            user_llvm_path,
            roc_modules,
            llvm_codegen_module,
            llvm_embedded_module,
            zstd,
        );
        if (snapshot_test.root_module.resolved_target.?.result.os.tag != .windows or
            snapshot_test.root_module.resolved_target.?.result.abi != .msvc)
        {
            snapshot_test.root_module.link_libcpp = true;
        }

        add_tracy(b, roc_modules.build_options, snapshot_test, target, true, flag_enable_tracy);
        build_test_zig_step.dependOn(&snapshot_test.step);

        const run_snapshot_test = b.addRunArtifact(snapshot_test);
        if (snapshot_exe_install) |install| {
            run_snapshot_test.step.dependOn(&install.step);
            run_snapshot_test.setEnvironmentVariable("ROC_SNAPSHOT_CHILD_EXE", b.getInstallPath(.bin, snapshot_exe.out_filename));
        }
        if (run_args.len != 0) {
            run_snapshot_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_snapshot_test.step);

        const run_snapshot_tool_test_step = b.step(
            "run-test-zig-snapshot-tool",
            "Run snapshot tool Zig tests",
        );
        run_snapshot_tool_test_step.dependOn(&run_snapshot_test.step);
    }

    // Add Builtin.roc doc code-block tests. Verifies every ```roc block in
    // src/build/roc/Builtin.roc passes the in-memory equivalent of
    // `roc check`, and either runs as `roc test` (when the block is only
    // top-level expects) or evaluates.
    const enable_builtin_doc_tests = b.option(bool, "builtin-doc-tests", "Enable Builtin.roc doc code-block tests") orelse true;
    if (enable_builtin_doc_tests) {
        const builtin_doc_test = b.addTest(.{
            .name = "builtin_doc_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/eval/test/builtin_doc_tests.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(builtin_doc_test);
        builtin_doc_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        builtin_doc_test.step.dependOn(&write_compiled_builtins.step);
        try addLlvmSupportToStep(
            b,
            builtin_doc_test,
            target,
            use_system_llvm,
            user_llvm_path,
            roc_modules,
            llvm_codegen_module,
            llvm_embedded_module,
            zstd,
        );
        if (builtin_doc_test.root_module.resolved_target.?.result.os.tag != .windows or
            builtin_doc_test.root_module.resolved_target.?.result.abi != .msvc)
        {
            builtin_doc_test.root_module.link_libcpp = true;
        }
        add_tracy(b, roc_modules.build_options, builtin_doc_test, target, true, flag_enable_tracy);
        build_test_zig_step.dependOn(&builtin_doc_test.step);

        const run_builtin_doc_test = b.addRunArtifact(builtin_doc_test);
        if (run_args.len != 0) {
            run_builtin_doc_test.addArgs(run_args);
        }

        tests_summary.addRun(&run_builtin_doc_test.step);

        const run_builtin_doc_test_step = b.step(
            "run-test-zig-builtin-doc",
            "Run Builtin.roc doc code-block Zig tests",
        );
        run_builtin_doc_test_step.dependOn(&run_builtin_doc_test.step);
    }

    const lir_inline_test = b.addTest(.{
        .name = "lir_inline_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/lir_inline_test.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .filters = test_filters,
    });
    roc_modules.addAll(lir_inline_test);
    lir_inline_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
    lir_inline_test.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        lir_inline_test,
        target,
        use_system_llvm,
        user_llvm_path,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (lir_inline_test.root_module.resolved_target.?.result.os.tag != .windows or
        lir_inline_test.root_module.resolved_target.?.result.abi != .msvc)
    {
        lir_inline_test.root_module.link_libcpp = true;
    }
    add_tracy(b, roc_modules.build_options, lir_inline_test, target, true, flag_enable_tracy);
    build_test_zig_step.dependOn(&lir_inline_test.step);

    const run_lir_inline_test = b.addRunArtifact(lir_inline_test);
    if (run_args.len != 0) {
        run_lir_inline_test.addArgs(run_args);
    }

    tests_summary.addRun(&run_lir_inline_test.step);

    const run_lir_inline_test_step = b.step(
        "run-test-zig-lir-inline",
        "Run LIR inline Zig tests",
    );
    run_lir_inline_test_step.dependOn(&run_lir_inline_test.step);

    // Add CLI test
    const enable_cli_tests = b.option(bool, "cli-tests", "Enable cli tests") orelse true;
    if (enable_cli_tests) {
        const cli_test = b.addTest(.{
            .name = "cli_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/main.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(cli_test);
        cli_test.root_module.linkLibrary(zstd.artifact("zstd"));
        try addLlvmSupportToStep(
            b,
            cli_test,
            target,
            use_system_llvm,
            user_llvm_path,
            roc_modules,
            llvm_codegen_module,
            llvm_embedded_module,
            zstd,
        );
        if (cli_test.root_module.resolved_target.?.result.os.tag != .windows or
            cli_test.root_module.resolved_target.?.result.abi != .msvc)
        {
            cli_test.root_module.link_libcpp = true;
        }
        add_tracy(b, roc_modules.build_options, cli_test, target, true, flag_enable_tracy);
        cli_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        cli_test.step.dependOn(&write_compiled_builtins.step);
        build_test_zig_step.dependOn(&cli_test.step);

        const run_cli_test = b.addRunArtifact(cli_test);
        if (run_args.len != 0) {
            run_cli_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_cli_test.step);

        const run_cli_main_test_step = b.step(
            "run-test-zig-cli-main",
            "Run roc CLI main Zig tests",
        );
        run_cli_main_test_step.dependOn(&run_cli_test.step);
    }

    // Add watch tests
    const enable_watch_tests = b.option(bool, "watch-tests", "Enable watch tests") orelse true;
    if (enable_watch_tests) {
        const watch_test = b.addTest(.{
            .name = "watch_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/watch/watch.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(watch_test);
        add_tracy(b, roc_modules.build_options, watch_test, target, false, flag_enable_tracy);

        // Link platform-specific libraries for file watching
        if (target.result.os.tag == .macos and target_is_native) {
            watch_test.root_module.linkFramework("CoreFoundation", .{});
            watch_test.root_module.linkFramework("CoreServices", .{});
        } else if (target.result.os.tag == .windows) {
            watch_test.root_module.linkSystemLibrary("kernel32", .{});
        }

        build_test_zig_step.dependOn(&watch_test.step);

        const run_watch_test = b.addRunArtifact(watch_test);
        if (run_args.len != 0) {
            run_watch_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_watch_test.step);

        const run_watch_cli_test_step = b.step(
            "run-test-zig-watch-cli",
            "Run watch command Zig tests",
        );
        run_watch_cli_test_step.dependOn(&run_watch_test.step);
    }

    // Add check for forbidden patterns in type checker code
    const check_patterns = CheckTypeCheckerPatternsStep.create(b);
    run_check_type_checker_patterns_step.dependOn(&check_patterns.step);

    // Add check for @enumFromInt(0) usage
    const check_enum_from_int = CheckEnumFromIntZeroStep.create(b);
    run_check_enum_from_int_zero_step.dependOn(&check_enum_from_int.step);

    // Add check for unused variable suppression patterns
    const check_unused = CheckUnusedSuppressionStep.create(b);
    run_check_unused_suppression_step.dependOn(&check_unused.step);

    // Add check that deleted post-check output/remapping APIs do not reappear
    const check_postcheck_architecture = CheckPostcheckArchitectureStep.create(b);
    run_check_postcheck_architecture_step.dependOn(&check_postcheck_architecture.step);

    // Add check that semantic compiler stages do not recover missing data.
    const run_semantic_audit = ci_steps.SemanticAuditStep.create(b);
    run_check_semantic_audit_step.dependOn(&run_semantic_audit.step);

    // Check for @panic and std.debug.panic in interpreter and builtins
    const check_panic = CheckPanicStep.create(b);
    run_check_panic_step.dependOn(&check_panic.step);

    // Add check for global stdio usage in CLI code
    const check_cli_stdio = CheckCliGlobalStdioStep.create(b);
    run_check_cli_global_stdio_step.dependOn(&check_cli_stdio.step);

    run_test_zig_step.dependOn(&tests_summary.step);

    b.default_step.dependOn(build_playground_step);
    {
        const install = playground_test_install;
        b.default_step.dependOn(&install.step);
    }

    // Fmt zig code.
    const fmt_paths = .{ "src", "build.zig" };
    const fmt = b.addFmt(.{ .paths = &fmt_paths });
    run_fmt_zig_step.dependOn(&fmt.step);

    const check_fmt = b.addFmt(.{ .paths = &fmt_paths, .check = true });
    run_check_zig_format_step.dependOn(&check_fmt.step);

    // Parser code coverage with kcov
    // Only supported on Linux ARM64 and macOS (kcov doesn't work on Windows)
    // TODO ZIG 16: re-check if DWARF bug is fixed — may be able to enable x86_64 coverage
    // Linux x86_64 is NOT supported due to Zig 0.15.2 generating invalid DWARF .debug_line
    // sections that cause kcov to fail (see CoverageSummaryStep comments for details)
    const is_linux_x86_64 = target.result.os.tag == .linux and target.result.cpu.arch == .x86_64;
    const is_coverage_supported = (target.result.os.tag == .linux or target.result.os.tag == .macos) and !is_linux_x86_64;
    if (is_coverage_supported and isNativeishOrMusl(target)) {
        // Get the kcov dependency and build it from source
        // lazyDependency returns null on first pass; Zig re-runs build() after fetching
        // TODO ZIG 16: re-check if lazy dependency bug is fixed — may be able to restructure this block
        // ALL coverage-related code must be inside this block due to Zig 0.15.2 lazy dependency bug
        // where dependencies added to a step outside the lazy block are not executed when the step
        // also has dependencies added inside the lazy block.
        if (b.lazyDependency("kcov", .{})) |kcov_dep| {
            // Create parse module unit tests for coverage
            // We only use the parse unit tests (not snapshot tool) because:
            // 1. They test the parser directly
            // 2. They don't require LLVM dependencies
            const parse_unit_test = b.addTest(.{
                .name = "parse_unit_coverage",
                .root_module = b.createModule(.{
                    .root_source_file = b.path("src/parse/mod.zig"),
                    .target = target,
                    .optimize = .Debug, // Debug required for DWARF debug info
                }),
            });
            roc_modules.addModuleDependencies(parse_unit_test, .parse);

            // Install all artifacts
            const install_parse_test = b.addInstallArtifact(parse_unit_test, .{});

            const kcov_exe = kcov_dep.artifact("kcov");
            const install_kcov = b.addInstallArtifact(kcov_exe, .{});

            // Create a step for building all coverage binaries
            const build_cov_tests = b.step("build-coverage-tests", "Build coverage test binaries to zig-out/bin/");
            build_cov_tests.dependOn(&install_parse_test.step);
            build_cov_tests.dependOn(&install_kcov.step);

            build_coverage_tools_step.dependOn(build_cov_tests);

            // Create output directories before running kcov
            const mkdir_step = b.addSystemCommand(&.{ "mkdir", "-p", "kcov-output/parser" });
            mkdir_step.setCwd(b.path("."));
            mkdir_step.step.dependOn(build_cov_tests);

            // On macOS, kcov needs to be codesigned to use task_for_pid
            // Codesign the installed binary since we run from zig-out/bin/
            if (target.result.os.tag == .macos) {
                const codesign = b.addSystemCommand(&.{"codesign"});
                codesign.setCwd(b.path("."));
                codesign.addArgs(&.{ "-s", "-", "--entitlements" });
                codesign.addFileArg(kcov_dep.path("osx-entitlements.xml"));
                codesign.addArgs(&.{ "-f", "zig-out/bin/kcov" });
                codesign.step.dependOn(&install_kcov.step);
                mkdir_step.step.dependOn(&codesign.step);
            }

            // Run kcov on parse unit tests
            const run_parse_coverage = b.addSystemCommand(&.{"zig-out/bin/kcov"});
            // kcov includes all compiled files (including zig stdlib) in coverage.
            // Use --include-pattern to filter to only src/parse files.
            run_parse_coverage.addArg("--include-pattern=/src/parse/");
            run_parse_coverage.addArgs(&.{
                "kcov-output/parser",
                "zig-out/bin/parse_unit_coverage",
            });
            run_parse_coverage.setCwd(b.path("."));
            run_parse_coverage.step.dependOn(&mkdir_step.step);
            run_parse_coverage.step.dependOn(&install_parse_test.step);

            // Add coverage summary step that parses kcov JSON output
            const summary_step = CoverageSummaryStep.create(b, "kcov-output/parser", "parse_unit_coverage");
            summary_step.step.dependOn(&run_parse_coverage.step);

            // Eval coverage: builds a separate binary with coverage=true (comptime),
            // which DCEs dev/wasm backends, disables fork isolation, and forces
            // single-threaded — so kcov can trace the interpreter in-process.
            // Run separately via: zig build run-coverage-eval
            {
                const coverage_eval_step = b.step("run-coverage-eval", "Run eval tests with kcov code coverage");

                // Build a coverage-specific binary with the coverage build option.
                const eval_coverage_exe = b.addExecutable(.{
                    .name = "eval-coverage-runner",
                    .root_module = b.createModule(.{
                        .root_source_file = b.path("src/eval/test/parallel_runner.zig"),
                        .target = target,
                        .optimize = optimize,
                        .link_libc = true,
                    }),
                });
                configureBackend(eval_coverage_exe, target);
                roc_modules.addAll(eval_coverage_exe);
                eval_coverage_exe.root_module.addOptions("coverage_options", blk: {
                    const opts = b.addOptions();
                    opts.addOption(bool, "coverage", true);
                    break :blk opts;
                });
                eval_coverage_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
                eval_coverage_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
                eval_coverage_exe.root_module.addImport("test_harness", b.createModule(.{
                    .root_source_file = b.path("src/build/test_harness.zig"),
                    .imports = &.{.{ .name = "collections", .module = roc_modules.collections }},
                }));
                eval_coverage_exe.step.dependOn(&write_compiled_builtins.step);
                try addLlvmSupportToStep(
                    b,
                    eval_coverage_exe,
                    target,
                    use_system_llvm,
                    user_llvm_path,
                    roc_modules,
                    llvm_codegen_module,
                    llvm_embedded_module,
                    zstd,
                );
                if (eval_coverage_exe.root_module.resolved_target.?.result.os.tag != .windows or
                    eval_coverage_exe.root_module.resolved_target.?.result.abi != .msvc)
                {
                    eval_coverage_exe.root_module.link_libcpp = true;
                }

                const install_coverage_runner = b.addInstallArtifact(eval_coverage_exe, .{});

                const mkdir_eval = b.addSystemCommand(&.{ "mkdir", "-p", "kcov-output/eval" });
                mkdir_eval.setCwd(b.path("."));
                mkdir_eval.step.dependOn(&install_coverage_runner.step);
                mkdir_eval.step.dependOn(&install_kcov.step);

                if (target.result.os.tag == .macos) {
                    // kcov needs codesigning on macOS to use task_for_pid
                    const eval_codesign = b.addSystemCommand(&.{"codesign"});
                    eval_codesign.setCwd(b.path("."));
                    eval_codesign.addArgs(&.{ "-s", "-", "--entitlements" });
                    eval_codesign.addFileArg(kcov_dep.path("osx-entitlements.xml"));
                    eval_codesign.addArgs(&.{ "-f", "zig-out/bin/kcov" });
                    eval_codesign.step.dependOn(&install_kcov.step);
                    mkdir_eval.step.dependOn(&eval_codesign.step);
                }

                const run_eval_coverage = b.addSystemCommand(&.{"zig-out/bin/kcov"});
                run_eval_coverage.addArg("--include-pattern=/src/eval/");
                run_eval_coverage.addArgs(&.{
                    "kcov-output/eval",
                    "zig-out/bin/eval-coverage-runner",
                });
                run_eval_coverage.setCwd(b.path("."));
                run_eval_coverage.step.dependOn(&mkdir_eval.step);
                run_eval_coverage.step.dependOn(&install_coverage_runner.step);
                run_eval_coverage.step.dependOn(&install_kcov.step);

                const eval_summary_step = CoverageSummaryStep.createWithOptions(b, "kcov-output/eval", "eval-coverage-runner", "EVAL", 0.0);
                eval_summary_step.step.dependOn(&run_eval_coverage.step);

                coverage_eval_step.dependOn(&eval_summary_step.step);
            }

            // Cross-compile for Windows to verify comptime branches compile
            // TODO ZIG 16: re-check if this lazy dependency bug is fixed
            // NOTE: This must be inside the lazy block due to Zig 0.15.2 bug where
            // dependencies added outside the lazy block prevent those inside from executing
            const windows_target = b.resolveTargetQuery(.{
                .cpu_arch = .x86_64,
                .os_tag = .windows,
                .abi = .msvc,
            });
            const windows_parse_build = b.addTest(.{
                .name = "parse_windows_comptime",
                .root_module = b.createModule(.{
                    .root_source_file = b.path("src/parse/mod.zig"),
                    .target = windows_target,
                    .optimize = .Debug,
                }),
            });
            roc_modules.addModuleDependencies(windows_parse_build, .parse);
            // Just compile, don't run - verifies Windows comptime branches
            build_coverage_tools_step.dependOn(&windows_parse_build.step);

            // Add explicit dependencies on install steps to the build step itself
            // TODO ZIG 16: re-check if lazy dependency issues are fixed
            // to work around Zig 0.15.2 lazy dependency issues
            build_coverage_tools_step.dependOn(&install_parse_test.step);
            build_coverage_tools_step.dependOn(&install_kcov.step);

            run_coverage_parser_step.dependOn(build_coverage_tools_step);
            run_coverage_parser_step.dependOn(&summary_step.step);
        }
    } else if (!is_coverage_supported) {
        // On unsupported platforms, print a message
        const unsupported_step = b.allocator.create(Step) catch @panic("OOM");
        unsupported_step.* = Step.init(.{
            .id = Step.Id.custom,
            .name = "coverage-unsupported",
            .owner = b,
            .makeFn = struct {
                fn make(_: *Step, _: Step.MakeOptions) !void {
                    std.debug.print("\n", .{});
                    std.debug.print("=" ** 60 ++ "\n", .{});
                    std.debug.print("COVERAGE NOT SUPPORTED\n", .{});
                    std.debug.print("=" ** 60 ++ "\n\n", .{});
                    std.debug.print("kcov parser coverage is currently enabled only on Linux ARM64 and macOS targets with supported Zig DWARF.\n", .{});
                    std.debug.print("Current platform: {s}\n\n", .{@tagName(builtin.target.os.tag)});
                    std.debug.print("=" ** 60 ++ "\n", .{});
                }
            }.make,
        });
        run_coverage_parser_step.dependOn(unsupported_step);
    }
    build_ci_step.dependOn(build_roc_step);
    build_ci_step.dependOn(build_check_tools_step);
    build_ci_step.dependOn(build_snapshot_tool_step);
    build_ci_step.dependOn(build_test_zig_step);
    build_ci_step.dependOn(build_test_lsp_integration_runner_step);
    build_ci_step.dependOn(build_test_eval_runner_step);
    build_ci_step.dependOn(build_test_eval_host_effects_runner_step);
    build_ci_step.dependOn(build_playground_step);
    build_ci_step.dependOn(build_test_playground_runner_step);
    build_ci_step.dependOn(build_test_cli_runners_step);
    build_ci_step.dependOn(build_test_hosts_step);
    build_ci_step.dependOn(build_test_serialization_sizes_step);
    build_ci_step.dependOn(build_test_wasm_static_lib_runner_step);
    build_ci_step.dependOn(build_coverage_tools_step);

    const fuzz = b.option(bool, "fuzz", "Build fuzz targets including AFL++ and tooling") orelse false;
    const is_windows = target.result.os.tag == .windows;

    // fx platform effectful functions test - only run when not cross-compiling
    if (isNativeishOrMusl(target)) {
        // Determine the appropriate target for the fx platform host library.
        // On Linux, we need to use musl explicitly because the CLI's findHostLibrary
        // looks for targets/x64musl/libhost.a first, and musl produces proper static binaries.
        const native_fx_target_dir = roc_target.RocTarget.fromStdTarget(target.result).toName();
        const fx_host_target, const fx_host_target_dir: ?[]const u8 = switch (target.result.os.tag) {
            .linux => switch (target.result.cpu.arch) {
                .x86_64 => .{ b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl }), "x64musl" },
                .aarch64 => .{ b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl }), "arm64musl" },
                else => .{ target, native_fx_target_dir },
            },
            .windows => switch (target.result.cpu.arch) {
                .x86_64 => .{ target, "x64win" },
                .aarch64 => .{ target, "arm64win" },
                else => .{ target, native_fx_target_dir },
            },
            else => .{ target, native_fx_target_dir },
        };

        // Create fx test platform host static library
        const test_platform_fx_host_lib = createTestPlatformHostLib(
            b,
            "test_platform_fx_host",
            "test/fx/platform/host.zig",
            fx_host_target,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
            .{ .uses_stack_handler = true },
        );

        // Copy the fx test platform host library to the source directory
        const copy_test_fx_host = b.addUpdateSourceFiles();
        const test_fx_host_filename = if (target.result.os.tag == .windows) "host.lib" else "libhost.a";
        const fx_host_main_path = b.pathJoin(&.{ "test/fx/platform", test_fx_host_filename });
        copy_test_fx_host.addCopyFileToSource(test_platform_fx_host_lib.getEmittedBin(), fx_host_main_path);

        // Also copy to the target-specific directory so findHostLibrary finds it
        const fx_host_target_path = if (fx_host_target_dir) |target_dir|
            b.pathJoin(&.{ "test/fx/platform/targets", target_dir, test_fx_host_filename })
        else
            null;
        if (fx_host_target_path) |target_path| {
            copy_test_fx_host.addCopyFileToSource(
                test_platform_fx_host_lib.getEmittedBin(),
                target_path,
            );
        }

        // Apply archive padding fix for non-Windows targets (Zig bug workaround)
        // The final_fx_host_step is what tests should depend on to ensure the archive is ready
        const final_fx_host_step: *Step = if (target.result.os.tag != .windows) blk: {
            const fix_main = FixArchivePaddingStep.create(b, fx_host_main_path);
            fix_main.step.dependOn(&copy_test_fx_host.step);

            if (fx_host_target_path) |target_path| {
                const fix_target = FixArchivePaddingStep.create(b, target_path);
                fix_target.step.dependOn(&copy_test_fx_host.step);
                // Make fix_target depend on fix_main so both complete
                fix_target.step.dependOn(&fix_main.step);
                break :blk &fix_target.step;
            }
            break :blk &fix_main.step;
        } else &copy_test_fx_host.step;

        b.getInstallStep().dependOn(final_fx_host_step);

        const static_data_host_target_dir = fx_host_target_dir orelse native_fx_target_dir;
        const final_static_data_host_step = buildAndCopyTestPlatformHostLib(
            b,
            "static-data-host",
            fx_host_target,
            static_data_host_target_dir,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
        );
        b.getInstallStep().dependOn(final_static_data_host_step);

        const final_static_data_platform_step: *Step = if (std.mem.endsWith(u8, static_data_host_target_dir, "musl")) blk: {
            const copy_musl_runtime = b.addUpdateSourceFiles();
            copy_musl_runtime.addCopyFileToSource(
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", static_data_host_target_dir, "crt1.o" })),
                b.pathJoin(&.{ "test/static-data-host/platform/targets", static_data_host_target_dir, "crt1.o" }),
            );
            copy_musl_runtime.addCopyFileToSource(
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", static_data_host_target_dir, "libc.a" })),
                b.pathJoin(&.{ "test/static-data-host/platform/targets", static_data_host_target_dir, "libc.a" }),
            );
            copy_musl_runtime.step.dependOn(final_static_data_host_step);
            break :blk &copy_musl_runtime.step;
        } else final_static_data_host_step;
        b.getInstallStep().dependOn(final_static_data_platform_step);

        const fx_platform_test = b.addTest(.{
            .name = "fx_platform_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/test/fx_platform_test.zig"),
                .target = target,
                .optimize = optimize,
                // util.buildIsolatedTestEnvMap touches std.c (Zig 0.16 requires explicit link_libc).
                .link_libc = true,
            }),
            .filters = test_filters,
        });

        const run_fx_platform_test = b.addRunArtifact(fx_platform_test);
        if (run_args.len != 0) {
            run_fx_platform_test.addArgs(run_args);
        }
        build_test_zig_step.dependOn(&fx_platform_test.step);
        // Ensure host library is copied AND fixed before running the test
        run_fx_platform_test.step.dependOn(final_fx_host_step);
        run_fx_platform_test.step.dependOn(final_static_data_platform_step);
        // Ensure roc binary is built before running the test (tests invoke roc CLI)
        run_fx_platform_test.step.dependOn(build_roc_step);
        tests_summary.addRun(&run_fx_platform_test.step);

        const run_fx_platform_zig_test_step = b.step(
            "run-test-zig-fx-platform",
            "Run fx platform Zig tests",
        );
        run_fx_platform_zig_test_step.dependOn(&run_fx_platform_test.step);
    }

    // Build glue platform host at runtime for the native platform.
    if (run_cli_test_step != null) {
        if (isNativeishOrMusl(target)) {
            // Determine the appropriate target for the glue platform host library.
            // On Linux, we need to use musl explicitly because the platform's
            // findHostLibrary looks for targets/x64musl/libhost.a.
            const glue_host_target, const glue_host_target_dir: ?[]const u8 = switch (target.result.os.tag) {
                .linux => switch (target.result.cpu.arch) {
                    .x86_64 => .{ b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl }), "x64musl" },
                    .aarch64 => .{ b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl }), "arm64musl" },
                    else => .{ target, null },
                },
                .windows => switch (target.result.cpu.arch) {
                    .x86_64 => .{ target, "x64win" },
                    .aarch64 => .{ target, "arm64win" },
                    else => .{ target, null },
                },
                .macos => switch (target.result.cpu.arch) {
                    .x86_64 => .{ target, "x64mac" },
                    .aarch64 => .{ target, "arm64mac" },
                    else => .{ target, null },
                },
                else => .{ target, null },
            };

            if (glue_host_target_dir) |target_dir| {
                const glue_platform_host_lib = createTestPlatformHostLib(
                    b,
                    "glue_platform_host_runtime",
                    "src/glue/platform/host.zig",
                    glue_host_target,
                    optimize,
                    roc_modules,
                    strip,
                    omit_frame_pointer,
                    .{ .uses_stack_handler = true },
                );

                // Add compiler modules to glue platform host for type extraction
                glue_platform_host_lib.root_module.addImport("can", roc_modules.can);
                glue_platform_host_lib.root_module.addImport("types", roc_modules.types);
                glue_platform_host_lib.root_module.addImport("layout", roc_modules.layout);
                glue_platform_host_lib.root_module.addImport("eval", roc_modules.eval);
                glue_platform_host_lib.root_module.addImport("collections", roc_modules.collections);

                // Copy to the target-specific directory
                const copy_glue_host = b.addUpdateSourceFiles();
                const glue_host_filename = if (target.result.os.tag == .windows) "host.lib" else "libhost.a";
                const target_path = b.pathJoin(&.{ "src/glue/platform/targets", target_dir, glue_host_filename });

                // Ensure the target directory exists
                const dir_path = b.pathJoin(&.{ "src/glue/platform/targets", target_dir });
                std.Io.Dir.cwd().createDirPath(b.graph.io, dir_path) catch {};

                copy_glue_host.addCopyFileToSource(glue_platform_host_lib.getEmittedBin(), target_path);

                // Apply archive padding fix for non-Windows targets
                const final_step: *Step = if (target.result.os.tag != .windows) blk: {
                    const fix_target = FixArchivePaddingStep.create(b, target_path);
                    fix_target.step.dependOn(&copy_glue_host.step);
                    break :blk &fix_target.step;
                } else &copy_glue_host.step;

                if (run_cli_test_step) |cli_test_step| {
                    cli_test_step.dependOn(final_step);
                }
            }
        }
    }

    var build_afl = false;
    if (!isNativeishOrMusl(target)) {
        std.log.warn("Cross compilation does not support fuzzing (Only building repro executables)", .{});
    } else if (is_windows) {
        // Windows does not support fuzzing - only build repro executables
    } else if (use_system_afl) {
        // If we have system afl, no need for llvm-config.
        build_afl = true;
    } else {
        // AFL++ does not work with our prebuilt static llvm.
        // Check for llvm-config program in user_llvm_path or on the system.
        // If found, let AFL++ use that.
        if (b.findProgram(&.{"llvm-config"}, &.{})) |_| {
            build_afl = true;
        } else |_| {
            std.log.warn("AFL++ requires a full version of llvm from the system or passed in via -Dllvm-path, but `llvm-config` was not found (Only building repro executables)", .{});
        }
    }

    const names: []const []const u8 = &.{
        "tokenize",
        "parse",
        "canonicalize",
    };
    for (names) |name| {
        add_fuzz_target(
            b,
            fuzz,
            build_afl,
            use_system_afl,
            no_bin,
            run_args,
            target,
            optimize,
            roc_modules,
            flag_enable_tracy,
            name,
        );
    }
}

fn discoverBuiltinRocFiles(b: *std.Build) ![]const []const u8 {
    const io = b.graph.io;
    const builtin_roc_path = try b.build_root.join(b.allocator, &.{ "src", "build", "roc" });
    var builtin_roc_dir = try std.Io.Dir.openDirAbsolute(io, builtin_roc_path, .{ .iterate = true });
    defer builtin_roc_dir.close(io);

    var roc_files = std.ArrayList([]const u8).empty;
    errdefer roc_files.deinit(b.allocator);

    var iter = builtin_roc_dir.iterate();
    while (try iter.next(io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            const full_path = b.fmt("src/build/roc/{s}", .{entry.name});
            try roc_files.append(b.allocator, full_path);
        }
    }

    return roc_files.toOwnedSlice(b.allocator);
}

fn add_fuzz_target(
    b: *std.Build,
    fuzz: bool,
    build_afl: bool,
    use_system_afl: bool,
    no_bin: bool,
    run_args: []const []const u8,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    tracy: ?[]const u8,
    name: []const u8,
) void {
    // We always include the repro scripts (no dependencies).
    // We only include the fuzzing scripts if `-Dfuzz` is set.
    const root_source_file = b.path(b.fmt("test/fuzzing/fuzz-{s}.zig", .{name}));
    const fuzz_obj = b.addObject(.{
        .name = b.fmt("{s}_obj", .{name}),
        .root_module = b.createModule(.{
            .root_source_file = root_source_file,
            .target = target,
            // Work around instrumentation bugs on mac without giving up perf on linux.
            .optimize = if (target.result.os.tag == .macos) .Debug else .ReleaseSafe,
        }),
    });
    configureBackend(fuzz_obj, target);
    // Required for fuzzing.
    fuzz_obj.root_module.link_libc = true;
    fuzz_obj.root_module.stack_check = false;
    // Enable coverage instrumentation for AFL++ when building fuzz targets.
    if (fuzz and build_afl) {
        fuzz_obj.sanitize_coverage_trace_pc_guard = true;
    }

    roc_modules.addAll(fuzz_obj);
    add_tracy(b, roc_modules.build_options, fuzz_obj, target, false, tracy);

    const name_exe = b.fmt("fuzz-{s}", .{name});
    const name_repro = b.fmt("repro-{s}", .{name});
    const build_repro_step = b.step(b.fmt("build-repro-{s}", .{name}), b.fmt("Build fuzz reproduction for {s}", .{name}));
    const run_repro_step = b.step(b.fmt("run-repro-{s}", .{name}), b.fmt("Run fuzz reproduction for {s}", .{name}));
    const repro_exe = b.addExecutable(.{
        .name = name_repro,
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/fuzzing/fuzz-repro.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(repro_exe, target);
    repro_exe.root_module.addImport("fuzz_test", fuzz_obj.root_module);

    _ = install_and_run(b, no_bin, repro_exe, build_repro_step, run_repro_step, run_args);

    if (fuzz and build_afl and !no_bin) {
        const fuzz_step = b.step(b.fmt("build-fuzz-{s}", .{name}), b.fmt("Build fuzz executable for {s}", .{name}));
        b.default_step.dependOn(fuzz_step);

        const afl = b.lazyImport(@This(), "afl_kit") orelse return;
        const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, &.{}, use_system_afl, fuzz_obj, &.{"-lm"}) orelse return;
        const install_fuzz = b.addInstallBinFile(fuzz_exe, name_exe);
        fuzz_step.dependOn(&install_fuzz.step);
        b.getInstallStep().dependOn(&install_fuzz.step);
    }
}

fn addMainExe(
    b: *std.Build,
    roc_modules: modules.RocModules,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    strip: bool,
    omit_frame_pointer: ?bool,
    use_system_llvm: bool,
    user_llvm_path: ?[]const u8,
    tracy: ?[]const u8,
    zstd: *Dependency,
    compiled_builtins_module: *std.Build.Module,
    write_compiled_builtins: *Step.WriteFile,
    llvm_codegen_module: *std.Build.Module,
    flag_enable_tracy: ?[]const u8,
) ?*Step.Compile {
    const exe = b.addExecutable(.{
        .name = "roc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .link_libc = true,
        }),
    });
    // The in-process interpreter (used by `--opt=interpreter`) recurses Zig stack
    // frames per Roc call. With Zig 0.16 codegen frame sizes, the Windows 1 MiB
    // default reserve isn't enough — recursion-heavy Roc programs trip our
    // SetUnhandledExceptionFilter stack-overflow handler before the interpreter
    // can catch the overflow itself. Reserve 64 MiB to match eval-test-runner.
    exe.stack_size = 64 * 1024 * 1024;
    configureBackend(exe, target);
    exe.root_module.addImport("llvm_codegen", llvm_codegen_module);

    // Build str and int test platform host libraries for native target
    // (fx and fx-open are only built by build-test-hosts for CLI platform tests)
    const main_build_platforms = [_][]const u8{ "str", "int" };
    const native_target_name = roc_target.RocTarget.fromStdTarget(target.result).toName();

    for (main_build_platforms) |platform_dir| {
        const copy_step = buildAndCopyTestPlatformHostLib(
            b,
            platform_dir,
            target,
            native_target_name,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
        );
        b.getInstallStep().dependOn(copy_step);
    }

    // Cross-compile for all Linux targets (musl + glibc)
    for (linux_cross_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        for (main_build_platforms) |platform_dir| {
            const copy_step = buildAndCopyTestPlatformHostLib(
                b,
                platform_dir,
                cross_resolved_target,
                cross_target.name,
                optimize,
                roc_modules,
                strip,
                omit_frame_pointer,
            );
            b.getInstallStep().dependOn(copy_step);
        }

        // Generate glibc stubs for gnu targets
        if (cross_target.query.abi == .gnu) {
            const glibc_stub = generateGlibcStub(b, cross_resolved_target, cross_target.name);
            if (glibc_stub) |stub| {
                b.getInstallStep().dependOn(&stub.step);
            }
        }
    }

    // Create builtins object file at build time with minimal dependencies.
    // This is a plain .o (not a .a archive) since we don't bundle compiler_rt here
    // (compiler_rt is bundled in the shim instead). Using .o avoids ar archive format
    // issues and is simpler since we pass it directly to the linker.
    const builtins_obj = b.addObject(.{
        .name = "roc_builtins",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
    });
    // Provide a no-op tracy stub so host_abi.zig can do @import("tracy") without
    // pulling in the real tracy module (which requires build_options).
    builtins_obj.root_module.addImport("tracy", b.addModule("tracy_stub", .{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins_obj.root_module.addImport("shim_io", b.addModule("shim_io", .{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins_obj.bundle_compiler_rt = false;
    configureBackend(builtins_obj, target);

    // Create shim static library at build time - fully static without libc
    //
    // NOTE we do NOT link libC here to avoid dynamic dependency on libC
    const shim_lib = b.addLibrary(.{
        .name = "roc_interpreter_shim",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/interpreter_shim/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
        .linkage = .static,
    });
    configureBackend(shim_lib, target);
    // Add all modules from roc_modules that the shim needs
    roc_modules.addAll(shim_lib);
    shim_lib.root_module.addImport("shim_io", b.addModule("shim_io", .{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    // Add compiled builtins module for loading builtin types
    shim_lib.root_module.addImport("compiled_builtins", compiled_builtins_module);
    shim_lib.step.dependOn(&write_compiled_builtins.step);
    // Include the pre-built builtins object
    shim_lib.root_module.addObjectFile(builtins_obj.getEmittedBin());
    shim_lib.bundle_compiler_rt = true;
    // Install shim library to the output directory
    const install_shim = b.addInstallArtifact(shim_lib, .{});
    b.getInstallStep().dependOn(&install_shim.step);
    // Copy the shim library to the src/ directory for embedding as binary data
    // This is because @embedFile happens at compile time and needs the file to exist already
    // and zig doesn't permit embedding files from directories outside the source tree.
    const copy_shim = b.addUpdateSourceFiles();
    const interpreter_shim_filename = if (target.result.os.tag == .windows) "roc_interpreter_shim.lib" else "libroc_interpreter_shim.a";
    copy_shim.addCopyFileToSource(shim_lib.getEmittedBin(), b.pathJoin(&.{ "src/cli", interpreter_shim_filename }));
    exe.step.dependOn(&copy_shim.step);

    // Copy builtins object for the host target for embedding into CLI
    // This is used by `roc build --opt=dev` to link the app object with builtins
    const copy_builtins = b.addUpdateSourceFiles();
    const host_builtins_filename = if (target.result.os.tag == .windows) "roc_builtins.obj" else "roc_builtins.o";
    copy_builtins.addCopyFileToSource(builtins_obj.getEmittedBin(), b.pathJoin(&.{ "src/cli", host_builtins_filename }));
    exe.step.dependOn(&copy_builtins.step);

    // Add tracy support (required by parse/can/check modules)
    add_tracy(b, roc_modules.build_options, shim_lib, b.graph.host, false, flag_enable_tracy);

    // Cross-compile builtins objects for all supported targets.
    // These are needed by `roc build --opt=dev --target=X` to link the app object with builtins.
    // The interpreter shim is built only for the native host target above.
    const cross_compile_builtins_targets = [_]struct { name: []const u8, query: std.Target.Query }{
        .{ .name = "x64musl", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl } },
        .{ .name = "arm64musl", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl } },
        .{ .name = "x64glibc", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu } },
        .{ .name = "arm64glibc", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu } },
        .{ .name = "wasm32", .query = .{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none } },
        .{ .name = "x64win", .query = .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .gnu } },
        .{ .name = "arm64win", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .gnu } },
        .{ .name = "x64mac", .query = .{ .cpu_arch = .x86_64, .os_tag = .macos, .abi = .none } },
        .{ .name = "arm64mac", .query = .{ .cpu_arch = .aarch64, .os_tag = .macos, .abi = .none } },
    };

    for (cross_compile_builtins_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        // Build builtins object file for this target.
        const cross_builtins_obj = b.addObject(.{
            .name = b.fmt("roc_builtins_{s}", .{cross_target.name}),
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/builtins/static_lib.zig"),
                .target = cross_resolved_target,
                .optimize = optimize,
                .strip = strip,
                .omit_frame_pointer = omit_frame_pointer,
                .pic = true,
            }),
        });
        // Provide a no-op tracy stub (same as for host builtins above)
        cross_builtins_obj.root_module.addImport("tracy", b.addModule(
            b.fmt("tracy_stub_{s}", .{cross_target.name}),
            .{ .root_source_file = b.path("src/builtins/tracy_stub.zig") },
        ));
        cross_builtins_obj.root_module.addImport("shim_io", b.addModule(
            b.fmt("shim_io_{s}", .{cross_target.name}),
            .{ .root_source_file = b.path("src/shim_io.zig") },
        ));
        cross_builtins_obj.bundle_compiler_rt = false;
        configureBackend(cross_builtins_obj, cross_resolved_target);

        // Copy builtins object for this target for embedding into CLI
        // Used by `roc build --opt=dev --target=X` to link the app object with builtins
        const builtins_ext = if (cross_target.query.os_tag == .windows) "roc_builtins.obj" else "roc_builtins.o";
        const copy_cross_builtins = b.addUpdateSourceFiles();
        copy_cross_builtins.addCopyFileToSource(
            cross_builtins_obj.getEmittedBin(),
            b.pathJoin(&.{ "src/cli/targets", cross_target.name, builtins_ext }),
        );
        exe.step.dependOn(&copy_cross_builtins.step);
    }

    const config = b.addOptions();
    config.addOption(bool, "llvm", true);
    exe.root_module.addOptions("config", config);
    exe.root_module.addAnonymousImport("legal_details", .{ .root_source_file = b.path("legal_details") });

    const llvm_paths_exe = llvmPaths(b, target, use_system_llvm, user_llvm_path) orelse return null;
    exe.root_module.addLibraryPath(.{ .cwd_relative = llvm_paths_exe.lib });
    exe.root_module.addIncludePath(.{ .cwd_relative = llvm_paths_exe.include });
    try addStaticLlvmOptionsToModule(exe.root_module);

    add_tracy(b, roc_modules.build_options, exe, target, true, tracy);

    exe.root_module.linkLibrary(zstd.artifact("zstd"));

    return exe;
}

fn install_and_run(
    b: *std.Build,
    no_bin: bool,
    exe: *Step.Compile,
    build_step: *Step,
    run_step: *Step,
    run_args: []const []const u8,
) ?*Step.InstallArtifact {
    if (run_step != build_step) {
        run_step.dependOn(build_step);
    }
    if (no_bin) {
        // No build, just build, don't actually install or run.
        build_step.dependOn(&exe.step);
        b.getInstallStep().dependOn(&exe.step);
        return null;
    } else {
        const install = b.addInstallArtifact(exe, .{});

        // Add a step to print success message after build completes
        const success_step = PrintBuildSuccessStep.create(b);
        success_step.step.dependOn(&install.step);
        build_step.dependOn(&success_step.step);

        b.getInstallStep().dependOn(&install.step);

        const run = b.addRunArtifact(exe);
        run.step.dependOn(&install.step);
        if (run_args.len != 0) {
            run.addArgs(run_args);
        }
        run_step.dependOn(&run.step);
        return install;
    }
}

fn addLlvmSupportToStep(
    b: *std.Build,
    step: *Step.Compile,
    target: ResolvedTarget,
    use_system_llvm: bool,
    user_llvm_path: ?[]const u8,
    roc_modules: anytype,
    llvm_codegen_module: *std.Build.Module,
    llvm_embedded_module: *std.Build.Module,
    zstd: *Dependency,
) !void {
    const llvm_paths = llvmPaths(b, target, use_system_llvm, user_llvm_path) orelse return;
    step.root_module.addLibraryPath(.{ .cwd_relative = llvm_paths.lib });
    step.root_module.addIncludePath(.{ .cwd_relative = llvm_paths.include });
    try addStaticLlvmOptionsToModule(step.root_module);
    step.root_module.addImport("llvm_codegen", llvm_codegen_module);
    step.root_module.addAnonymousImport("llvm_compile", .{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
        .imports = &.{
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "layout", .module = roc_modules.layout },
            .{ .name = "backend", .module = roc_modules.backend },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "llvm_codegen", .module = llvm_codegen_module },
            .{ .name = "build_options", .module = roc_modules.build_options },
            .{ .name = "llvm_embedded", .module = llvm_embedded_module },
            .{ .name = "embedded_lld", .module = roc_modules.embedded_lld },
        },
    });
    step.root_module.linkLibrary(zstd.artifact("zstd"));
}

const ParsedBuildArgs = struct {
    run_args: []const []const u8,
    test_filters: []const []const u8,
};

fn appendFilter(
    list: *std.ArrayList([]const u8),
    b: *std.Build,
    value: []const u8,
) void {
    const trimmed = std.mem.trim(u8, value, " \t\n\r");
    if (trimmed.len == 0) return;
    list.append(b.allocator, b.dupe(trimmed)) catch @panic("OOM while parsing --test-filter value");
}

fn parseBuildArgs(b: *std.Build) ParsedBuildArgs {
    const raw_args = b.args orelse return .{
        .run_args = &.{},
        .test_filters = &.{},
    };

    var run_args_list = std.ArrayList([]const u8).empty;
    var filter_list = std.ArrayList([]const u8).empty;

    var i: usize = 0;
    while (i < raw_args.len) {
        const arg = raw_args[i];

        if (std.mem.eql(u8, arg, "--test-filter")) {
            i += 1;
            if (i >= raw_args.len) {
                std.log.warn("ignoring --test-filter with no value", .{});
                break;
            }
            const value = raw_args[i];
            appendFilter(&filter_list, b, value);
            i += 1;
            continue;
        }

        if (std.mem.startsWith(u8, arg, "--test-filter=")) {
            const value = arg["--test-filter=".len..];
            appendFilter(&filter_list, b, value);
            i += 1;
            continue;
        }

        run_args_list.append(b.allocator, arg) catch @panic("OOM while recording build arguments");
        i += 1;
    }

    const run_args = run_args_list.toOwnedSlice(b.allocator) catch @panic("OOM while finalizing build arguments");
    const test_filters = filter_list.toOwnedSlice(b.allocator) catch @panic("OOM while finalizing test filters");

    return .{ .run_args = run_args, .test_filters = test_filters };
}

fn add_tracy(
    b: *std.Build,
    module_build_options: *std.Build.Module,
    base: *Step.Compile,
    target: ResolvedTarget,
    links_llvm: bool,
    tracy: ?[]const u8,
) void {
    base.root_module.addImport("build_options", module_build_options);
    if (tracy) |tracy_path| {
        const client_cpp = b.pathJoin(
            &[_][]const u8{ tracy_path, "public", "TracyClient.cpp" },
        );

        // On mingw, we need to opt into windows 7+ to get some features required by tracy.
        const tracy_c_flags: []const []const u8 = if (target.result.os.tag == .windows and target.result.abi == .gnu)
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
        else
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };

        base.root_module.addIncludePath(.{ .cwd_relative = tracy_path });
        base.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = client_cpp }, .flags = tracy_c_flags });
        base.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = "src/build/tracy-shutdown.cpp" }, .flags = tracy_c_flags });
        if (!links_llvm) {
            base.root_module.linkSystemLibrary("c++", .{ .use_pkg_config = .no });
        }
        base.root_module.link_libc = true;

        if (target.result.os.tag == .windows) {
            base.root_module.linkSystemLibrary("dbghelp", .{});
            base.root_module.linkSystemLibrary("ws2_32", .{});
        }
    }
}

const LlvmPaths = struct {
    include: []const u8,
    lib: []const u8,
};

/// Get LLVM library and include paths.
/// Priority:
/// 1. If user_llvm_path is provided, use that
/// 2. If use_system_llvm is true, detect system LLVM via llvm-config
/// 3. Otherwise, download from roc-bootstrap (default)
fn llvmPaths(
    b: *std.Build,
    target: ResolvedTarget,
    use_system_llvm: bool,
    user_llvm_path: ?[]const u8,
) ?LlvmPaths {
    if (use_system_llvm and user_llvm_path != null) {
        std.log.err("-Dsystem-llvm and -Dllvm-path cannot both be specified", .{});
        std.process.exit(1);
    }

    if (user_llvm_path) |llvm_path| {
        // User specified a custom LLVM path
        return .{
            .include = b.pathJoin(&.{ llvm_path, "include" }),
            .lib = b.pathJoin(&.{ llvm_path, "lib" }),
        };
    }

    if (use_system_llvm) {
        // Detect system LLVM via llvm-config (required for AFL++)
        const llvm_config_path = b.findProgram(&.{"llvm-config"}, &.{""}) catch {
            std.log.err("Failed to find system llvm-config binary. Is LLVM installed?", .{});
            std.process.exit(1);
        };
        const llvm_lib_dir = std.mem.trimEnd(u8, b.run(&.{ llvm_config_path, "--libdir" }), "\n");
        const llvm_include_dir = std.mem.trimEnd(u8, b.run(&.{ llvm_config_path, "--includedir" }), "\n");

        return .{
            .include = llvm_include_dir,
            .lib = llvm_lib_dir,
        };
    }

    // Default: download from roc-bootstrap
    const raw_triple = target.result.linuxTriple(b.allocator) catch @panic("OOM");
    if (!supported_deps_triples.has(raw_triple)) {
        std.log.err("Target triple({s}) not supported by roc-bootstrap.\n", .{raw_triple});
        std.log.err("Please specify `-Dsystem-llvm` or `-Dllvm-path` to provide a custom LLVM installation.\n", .{});
        std.process.exit(1);
    }
    const triple = supported_deps_triples.get(raw_triple).?;
    const deps_name = b.fmt("roc_deps_{s}", .{triple});
    const deps = b.lazyDependency(deps_name, .{}) orelse return null;
    const lazy_llvm_path = deps.path(".");
    // TODO: Is this ok to do in the zig build system?
    // We aren't in the make phase, but our static dep doesn't have a make phase anyway.
    // Not sure how else to get a static path to the downloaded dependency.
    const llvm_path = lazy_llvm_path.getPath(deps.builder);
    return .{
        .include = b.pathJoin(&.{ llvm_path, "include" }),
        .lib = b.pathJoin(&.{ llvm_path, "lib" }),
    };
}

const supported_deps_triples = std.StaticStringMap([]const u8).initComptime(.{
    .{ "aarch64-macos-none", "aarch64_macos_none" },
    .{ "aarch64-linux-musl", "aarch64_linux_musl" },
    .{ "aarch64-windows-gnu", "aarch64_windows_gnu" },
    .{ "arm-linux-musleabihf", "arm_linux_musleabihf" },
    .{ "x86-linux-musl", "x86_linux_musl" },
    .{ "x86_64-linux-musl", "x86_64_linux_musl" },
    .{ "x86_64-macos-none", "x86_64_macos_none" },
    .{ "x86_64-windows-gnu", "x86_64_windows_gnu" },
    // We also support the gnu linux targets.
    // For those, we just map to musl.
    .{ "aarch64-linux-gnu", "aarch64_linux_musl" },
    .{ "arm-linux-gnueabihf", "arm_linux_musleabihf" },
    .{ "x86-linux-gnu", "x86_linux_musl" },
    .{ "x86_64-linux-gnu", "x86_64_linux_musl" },
});

// The following is adapted from the Zig compiler at https://codeberg.org/ziglang/zig and licensed under the MIT license. Thanks, Zig team!
fn addStaticLlvmOptionsToModule(mod: *std.Build.Module) !void {
    const cpp_cflags = exe_cflags ++ [_][]const u8{"-DNDEBUG=1"};
    mod.addCSourceFiles(.{
        .files = &cpp_sources,
        .flags = &cpp_cflags,
    });

    const link_static = std.Build.Module.LinkSystemLibraryOptions{
        .preferred_link_mode = .static,
        .search_strategy = .mode_first,
    };
    for (lld_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, link_static);
    }

    for (llvm_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, link_static);
    }

    mod.linkSystemLibrary("z", link_static);

    if (mod.resolved_target.?.result.os.tag != .windows or mod.resolved_target.?.result.abi != .msvc) {
        // Use Zig's bundled static libc++ to keep the binary statically linked
        mod.link_libcpp = true;
    }

    if (mod.resolved_target.?.result.os.tag == .windows) {
        mod.linkSystemLibrary("ws2_32", .{});
        mod.linkSystemLibrary("version", .{});
        mod.linkSystemLibrary("uuid", .{});
        mod.linkSystemLibrary("ole32", .{});
    }
}

const cpp_sources = [_][]const u8{
    "src/build/zig_llvm.cpp",
};

const exe_cflags = [_][]const u8{
    "-std=c++17",
    "-D__STDC_CONSTANT_MACROS",
    "-D__STDC_FORMAT_MACROS",
    "-D__STDC_LIMIT_MACROS",
    "-D_GNU_SOURCE",
    "-fno-exceptions",
    "-fno-rtti",
    "-fno-stack-protector",
    "-fvisibility-inlines-hidden",
    "-Wno-type-limits",
    "-Wno-missing-braces",
    "-Wno-comment",
};
const lld_libs = [_][]const u8{
    "lldMinGW",
    "lldELF",
    "lldCOFF",
    "lldWasm",
    "lldMachO",
    "lldCommon",
};
// This list can be re-generated with `llvm-config --libfiles` and then
// reformatting using your favorite text editor. Note we do not execute
// `llvm-config` here because we are cross compiling. Also omit LLVMTableGen
// from these libs.
const llvm_libs = [_][]const u8{
    "LLVMWindowsManifest",
    "LLVMXRay",
    "LLVMLibDriver",
    "LLVMDlltoolDriver",
    "LLVMTextAPIBinaryReader",
    "LLVMCoverage",
    "LLVMLineEditor",
    "LLVMXCoreDisassembler",
    "LLVMXCoreCodeGen",
    "LLVMXCoreDesc",
    "LLVMXCoreInfo",
    "LLVMX86TargetMCA",
    "LLVMX86Disassembler",
    "LLVMX86AsmParser",
    "LLVMX86CodeGen",
    "LLVMX86Desc",
    "LLVMX86Info",
    "LLVMWebAssemblyDisassembler",
    "LLVMWebAssemblyAsmParser",
    "LLVMWebAssemblyCodeGen",
    "LLVMWebAssemblyUtils",
    "LLVMWebAssemblyDesc",
    "LLVMWebAssemblyInfo",
    "LLVMVEDisassembler",
    "LLVMVEAsmParser",
    "LLVMVECodeGen",
    "LLVMVEDesc",
    "LLVMVEInfo",
    "LLVMSystemZDisassembler",
    "LLVMSystemZAsmParser",
    "LLVMSystemZCodeGen",
    "LLVMSystemZDesc",
    "LLVMSystemZInfo",
    "LLVMSparcDisassembler",
    "LLVMSparcAsmParser",
    "LLVMSparcCodeGen",
    "LLVMSparcDesc",
    "LLVMSparcInfo",
    "LLVMRISCVTargetMCA",
    "LLVMRISCVDisassembler",
    "LLVMRISCVAsmParser",
    "LLVMRISCVCodeGen",
    "LLVMRISCVDesc",
    "LLVMRISCVInfo",
    "LLVMPowerPCDisassembler",
    "LLVMPowerPCAsmParser",
    "LLVMPowerPCCodeGen",
    "LLVMPowerPCDesc",
    "LLVMPowerPCInfo",
    "LLVMNVPTXCodeGen",
    "LLVMNVPTXDesc",
    "LLVMNVPTXInfo",
    "LLVMSPIRVAnalysis",
    "LLVMSPIRVCodeGen",
    "LLVMSPIRVDesc",
    "LLVMSPIRVInfo",
    "LLVMMSP430Disassembler",
    "LLVMMSP430AsmParser",
    "LLVMMSP430CodeGen",
    "LLVMMSP430Desc",
    "LLVMMSP430Info",
    "LLVMMipsDisassembler",
    "LLVMMipsAsmParser",
    "LLVMMipsCodeGen",
    "LLVMMipsDesc",
    "LLVMMipsInfo",
    "LLVMLoongArchDisassembler",
    "LLVMLoongArchAsmParser",
    "LLVMLoongArchCodeGen",
    "LLVMLoongArchDesc",
    "LLVMLoongArchInfo",
    "LLVMLanaiDisassembler",
    "LLVMLanaiCodeGen",
    "LLVMLanaiAsmParser",
    "LLVMLanaiDesc",
    "LLVMLanaiInfo",
    "LLVMHexagonDisassembler",
    "LLVMHexagonCodeGen",
    "LLVMHexagonAsmParser",
    "LLVMHexagonDesc",
    "LLVMHexagonInfo",
    "LLVMBPFDisassembler",
    "LLVMBPFAsmParser",
    "LLVMBPFCodeGen",
    "LLVMBPFDesc",
    "LLVMBPFInfo",
    "LLVMAVRDisassembler",
    "LLVMAVRAsmParser",
    "LLVMAVRCodeGen",
    "LLVMAVRDesc",
    "LLVMAVRInfo",
    "LLVMARMDisassembler",
    "LLVMARMAsmParser",
    "LLVMARMCodeGen",
    "LLVMARMDesc",
    "LLVMARMUtils",
    "LLVMARMInfo",
    "LLVMAMDGPUTargetMCA",
    "LLVMAMDGPUDisassembler",
    "LLVMAMDGPUAsmParser",
    "LLVMAMDGPUCodeGen",
    "LLVMAMDGPUDesc",
    "LLVMAMDGPUUtils",
    "LLVMAMDGPUInfo",
    "LLVMAArch64Disassembler",
    "LLVMAArch64AsmParser",
    "LLVMAArch64CodeGen",
    "LLVMAArch64Desc",
    "LLVMAArch64Utils",
    "LLVMAArch64Info",
    "LLVMOrcDebugging",
    "LLVMOrcJIT",
    "LLVMWindowsDriver",
    "LLVMMCJIT",
    "LLVMJITLink",
    "LLVMInterpreter",
    "LLVMExecutionEngine",
    "LLVMRuntimeDyld",
    "LLVMOrcTargetProcess",
    "LLVMOrcShared",
    "LLVMDWP",
    "LLVMDebugInfoLogicalView",
    "LLVMDebugInfoGSYM",
    "LLVMOption",
    "LLVMObjectYAML",
    "LLVMObjCopy",
    "LLVMMCA",
    "LLVMMCDisassembler",
    "LLVMLTO",
    "LLVMPasses",
    "LLVMCGData",
    "LLVMHipStdPar",
    "LLVMCFGuard",
    "LLVMCoroutines",
    "LLVMSandboxIR",
    "LLVMipo",
    "LLVMVectorize",
    "LLVMLinker",
    "LLVMInstrumentation",
    "LLVMFrontendOpenMP",
    "LLVMFrontendAtomic",
    "LLVMFrontendOffloading",
    "LLVMFrontendOpenACC",
    "LLVMFrontendHLSL",
    "LLVMFrontendDriver",
    "LLVMExtensions",
    "LLVMDWARFLinkerParallel",
    "LLVMDWARFLinkerClassic",
    "LLVMDWARFLinker",
    "LLVMGlobalISel",
    "LLVMMIRParser",
    "LLVMAsmPrinter",
    "LLVMSelectionDAG",
    "LLVMCodeGen",
    "LLVMTarget",
    "LLVMObjCARCOpts",
    "LLVMCodeGenTypes",
    "LLVMIRPrinter",
    "LLVMInterfaceStub",
    "LLVMFileCheck",
    "LLVMFuzzMutate",
    "LLVMScalarOpts",
    "LLVMInstCombine",
    "LLVMAggressiveInstCombine",
    "LLVMTransformUtils",
    "LLVMBitWriter",
    "LLVMAnalysis",
    "LLVMProfileData",
    "LLVMSymbolize",
    "LLVMDebugInfoBTF",
    "LLVMDebugInfoPDB",
    "LLVMDebugInfoMSF",
    "LLVMDebugInfoDWARF",
    "LLVMDebugInfoDWARFLowLevel",
    "LLVMObject",
    "LLVMTextAPI",
    "LLVMMCParser",
    "LLVMIRReader",
    "LLVMAsmParser",
    "LLVMMC",
    "LLVMDebugInfoCodeView",
    "LLVMBitReader",
    "LLVMFuzzerCLI",
    "LLVMCore",
    "LLVMRemarks",
    "LLVMBitstreamReader",
    "LLVMBinaryFormat",
    "LLVMTargetParser",
    "LLVMSupport",
    "LLVMDemangle",
};

/// Get the git-commit component of the compiler version (e.g. "abc12345"), used for cache
/// versioning. Falls back to "no-git" when git is unavailable. The human-readable build-mode
/// prefix (e.g. "release-fast-") is prepended at the binary's compile time from
/// @import("builtin").mode — see where `compiler_version` is assembled in build().
fn getCompilerVersionGit(b: *std.Build) []const u8 {
    // Try to get git commit SHA using std.process.run
    const result = std.process.run(b.allocator, b.graph.io, .{
        .argv = &[_][]const u8{ "git", "rev-parse", "--short=8", "HEAD" },
    }) catch {
        // Git command failed, use fallback
        return "no-git";
    };
    defer b.allocator.free(result.stdout);
    defer b.allocator.free(result.stderr);

    if (result.term == .exited and result.term.exited == 0) {
        // Git succeeded, use the commit SHA (dupe it since result.stdout is freed above)
        const commit_sha = std.mem.trim(u8, result.stdout, " \n\r\t");
        if (commit_sha.len > 0) {
            return b.allocator.dupe(u8, commit_sha) catch "no-git";
        }
    }

    // Git not available or failed, use fallback
    return "no-git";
}

/// Return the semantic checked-artifact compiler hash.
///
/// This is intentionally one build-time hash. Checked artifact cache keys must
/// not separately store compiler version, builtin identity, semantic build
/// switches, or serialization format identity.
fn getCompilerArtifactHash(b: *std.Build, compiler_version: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc-checked-artifact-v1");
    hasher.update(compiler_version);

    const builtin_source = std.Io.Dir.cwd().readFileAlloc(
        b.graph.io,
        "src/build/roc/Builtin.roc",
        b.allocator,
        std.Io.Limit.limited(32 * 1024 * 1024),
    ) catch @panic("unable to read Builtin.roc while constructing compiler artifact hash");
    defer b.allocator.free(builtin_source);
    hasher.update(builtin_source);

    return hasher.finalResult();
}

/// Generate glibc stubs at build time for cross-compilation
///
/// This is a minimal implementation that generates essential symbols needed for basic
/// cross-compilation to glibc targets. It creates assembly stubs with required symbols
/// like __libc_start_main, abort, getauxval, and _IO_stdin_used.
///
/// Future work: Parse Zig's abilists file to generate comprehensive
/// symbol coverage with proper versioning (e.g., symbol@@GLIBC_2.17). The abilists
/// contains thousands of glibc symbols across different versions and architectures
/// that could provide more complete stub coverage for complex applications.
fn generateGlibcStub(b: *std.Build, target: ResolvedTarget, target_name: []const u8) ?*Step.UpdateSourceFiles {

    // Generate assembly stub with comprehensive symbols using the new build module
    var assembly_buf = std.ArrayList(u8).empty;
    defer assembly_buf.deinit(b.allocator);

    var aw = std.Io.Writer.Allocating.fromArrayList(b.allocator, &assembly_buf);
    const target_arch = target.result.cpu.arch;

    glibc_stub_build.generateComprehensiveStub(&aw.writer, target_arch) catch |err| {
        std.log.warn("Failed to generate comprehensive stub assembly for {s}: {}, using minimal ELF", .{ target_name, err });
        // Fall back to minimal ELF
        const stub_content = switch (target.result.cpu.arch) {
            .aarch64 => createMinimalElfArm64(),
            .x86_64 => createMinimalElfX64(),
            else => return null,
        };

        const write_stub = b.addWriteFiles();
        const libc_so_6 = write_stub.add("libc.so.6", stub_content);
        const libc_so = write_stub.add("libc.so", stub_content);

        const copy_stubs = b.addUpdateSourceFiles();
        // Platforms that need glibc stubs
        const glibc_platforms = [_][]const u8{ "int", "str" };
        for (glibc_platforms) |platform| {
            copy_stubs.addCopyFileToSource(libc_so_6, b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so.6" }));
            copy_stubs.addCopyFileToSource(libc_so, b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so" }));
        }
        copy_stubs.step.dependOn(&write_stub.step);

        return copy_stubs;
    };

    // Write the assembly file to the targets directory
    const write_stub = b.addWriteFiles();
    assembly_buf = aw.toArrayList();
    const asm_file = write_stub.add("libc_stub.s", assembly_buf.items);

    // Compile the assembly into a proper shared library using Zig's build system
    const libc_stub = glibc_stub_build.compileAssemblyStub(b, asm_file, target, .ReleaseSmall);

    // Copy the generated files to all platforms that use glibc targets
    const copy_stubs = b.addUpdateSourceFiles();

    // Platforms that need glibc stubs (have glibc targets defined in their .roc files)
    const glibc_platforms = [_][]const u8{ "int", "str" };
    for (glibc_platforms) |platform| {
        copy_stubs.addCopyFileToSource(libc_stub.getEmittedBin(), b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so.6" }));
        copy_stubs.addCopyFileToSource(libc_stub.getEmittedBin(), b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so" }));
        copy_stubs.addCopyFileToSource(asm_file, b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc_stub.s" }));
    }
    copy_stubs.step.dependOn(&libc_stub.step);
    copy_stubs.step.dependOn(&write_stub.step);

    return copy_stubs;
}

/// Create a minimal ELF shared object for ARM64
fn createMinimalElfArm64() []const u8 {
    // ARM64 minimal ELF shared object
    return &[_]u8{
        // ELF Header (64 bytes)
        0x7F, 'E', 'L', 'F', // e_ident[EI_MAG0..3] - ELF magic
        2, // e_ident[EI_CLASS] - ELFCLASS64
        1, // e_ident[EI_DATA] - ELFDATA2LSB (little endian)
        1, // e_ident[EI_VERSION] - EV_CURRENT
        0, // e_ident[EI_OSABI] - ELFOSABI_NONE
        0, // e_ident[EI_ABIVERSION]
        0, 0, 0, 0, 0, 0, 0, // e_ident[EI_PAD] - padding
        0x03, 0x00, // e_type - ET_DYN (shared object)
        0xB7, 0x00, // e_machine - EM_AARCH64
        0x01, 0x00, 0x00, 0x00, // e_version - EV_CURRENT
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_entry (not used for shared obj)
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_phoff - program header offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_shoff - section header offset
        0x00, 0x00, 0x00, 0x00, // e_flags
        0x40, 0x00, // e_ehsize - ELF header size
        0x38, 0x00, // e_phentsize - program header entry size
        0x01, 0x00, // e_phnum - number of program headers
        0x40, 0x00, // e_shentsize - section header entry size
        0x00, 0x00, // e_shnum - number of section headers
        0x00, 0x00, // e_shstrndx - section header string table index

        // Program Header (56 bytes) - PT_LOAD
        0x01, 0x00, 0x00, 0x00, // p_type - PT_LOAD
        0x05, 0x00, 0x00, 0x00, // p_flags - PF_R | PF_X
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_paddr
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_filesz
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_memsz
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_align
    };
}

/// Create a minimal ELF shared object for x86-64
fn createMinimalElfX64() []const u8 {
    // x86-64 minimal ELF shared object
    return &[_]u8{
        // ELF Header (64 bytes)
        0x7F, 'E', 'L', 'F', // e_ident[EI_MAG0..3] - ELF magic
        2, // e_ident[EI_CLASS] - ELFCLASS64
        1, // e_ident[EI_DATA] - ELFDATA2LSB (little endian)
        1, // e_ident[EI_VERSION] - EV_CURRENT
        0, // e_ident[EI_OSABI] - ELFOSABI_NONE
        0, // e_ident[EI_ABIVERSION]
        0, 0, 0, 0, 0, 0, 0, // e_ident[EI_PAD] - padding
        0x03, 0x00, // e_type - ET_DYN (shared object)
        0x3E, 0x00, // e_machine - EM_X86_64
        0x01, 0x00, 0x00, 0x00, // e_version - EV_CURRENT
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_entry (not used for shared obj)
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_phoff - program header offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_shoff - section header offset
        0x00, 0x00, 0x00, 0x00, // e_flags
        0x40, 0x00, // e_ehsize - ELF header size
        0x38, 0x00, // e_phentsize - program header entry size
        0x01, 0x00, // e_phnum - number of program headers
        0x40, 0x00, // e_shentsize - section header entry size
        0x00, 0x00, // e_shnum - number of section headers
        0x00, 0x00, // e_shstrndx - section header string table index

        // Program Header (56 bytes) - PT_LOAD
        0x01, 0x00, 0x00, 0x00, // p_type - PT_LOAD
        0x05, 0x00, 0x00, 0x00, // p_flags - PF_R | PF_X
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_paddr
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_filesz
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_memsz
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_align
    };
}
