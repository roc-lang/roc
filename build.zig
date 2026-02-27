const std = @import("std");
const builtin = @import("builtin");
const modules = @import("src/build/modules.zig");
const glibc_stub_build = @import("src/build/glibc_stub.zig");
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
const all_test_platform_dirs = [_][]const u8{ "str", "int", "fx", "fx-open" };

fn mustUseLlvm(target: ResolvedTarget) bool {
    return target.result.os.tag == .macos and target.result.cpu.arch == .x86_64;
}

fn configureBackend(step: *Step.Compile, target: ResolvedTarget) void {
    if (mustUseLlvm(target)) {
        step.use_llvm = true;
    }
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
    forced_passes: u64,

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
            .forced_passes = @intCast(forced_passes),
        };
        return self;
    }

    fn addRun(self: *TestsSummaryStep, run_step: *Step) void {
        self.step.dependOn(run_step);
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
            var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
                return step.fail("Failed to open {s} directory: {}", .{ dir_path, err });
            };
            defer dir.close();

            try scanDirectory(allocator, dir, dir_path, &violations);
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
        dir: std.fs.Dir,
        path_prefix: []const u8,
        violations: *std.ArrayList(Violation),
    ) !void {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

            // Skip test files - they may legitimately need string comparison for assertions
            if (std.mem.endsWith(u8, entry.path, "_test.zig")) continue;
            if (std.mem.indexOf(u8, entry.path, "test/") != null) continue;
            if (std.mem.startsWith(u8, entry.path, "test")) continue;
            if (std.mem.endsWith(u8, entry.path, "test_runner.zig")) continue;

            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

            const file = dir.openFile(entry.path, .{}) catch continue;
            defer file.close();

            const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch continue;
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
                    if (std.mem.indexOf(u8, line, "std.mem.")) |idx| {
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
                    if (std.mem.indexOf(u8, line, "findByString") != null and !isInExcludedRange(full_path, line_number)) {
                        try violations.append(allocator, .{
                            .file_path = full_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                        });
                    }

                    // Check for findIdent usage - should use pre-stored Ident.Idx instead
                    if (std.mem.indexOf(u8, line, "findIdent") != null and !isInExcludedRange(full_path, line_number)) {
                        try violations.append(allocator, .{
                            .file_path = full_path,
                            .line_number = line_number,
                            .line_content = try allocator.dupe(u8, trimmed),
                        });
                    }

                    // Check for getMethodIdent usage - should use pre-stored Ident.Idx instead
                    if (std.mem.indexOf(u8, line, "getMethodIdent") != null and !isInExcludedRange(full_path, line_number)) {
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
        var dir = std.fs.cwd().openDir("src", .{ .iterate = true }) catch |err| {
            return step.fail("Failed to open src directory: {}", .{err});
        };
        defer dir.close();

        try scanDirectoryForEnumFromIntZero(allocator, dir, "src", &violations);

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

    // Files in backend/llvm that are copies from Zig's stdlib and should be excluded
    const stdlib_copies = [_][]const u8{
        "backend/llvm/Builder.zig",
        "backend/llvm/ir.zig",
        "backend/llvm/bitcode_writer.zig",
        "backend/llvm/BitcodeReader.zig",
    };

    fn isStdlibCopy(path: []const u8) bool {
        for (stdlib_copies) |excluded| {
            if (std.mem.endsWith(u8, path, excluded)) return true;
        }
        return false;
    }

    fn scanDirectoryForEnumFromIntZero(
        allocator: std.mem.Allocator,
        dir: std.fs.Dir,
        path_prefix: []const u8,
        violations: *std.ArrayList(Violation),
    ) !void {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

            // Skip Zig stdlib copies in backend/llvm
            if (isStdlibCopy(full_path)) continue;

            const file = dir.openFile(entry.path, .{}) catch continue;
            defer file.close();

            const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch continue;
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

                    // Check for @enumFromInt(0) usage
                    if (std.mem.indexOf(u8, line, "@enumFromInt(0)") != null) {
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
        var dir = std.fs.cwd().openDir("src", .{ .iterate = true }) catch |err| {
            return step.fail("Failed to open src/ directory: {}", .{err});
        };
        defer dir.close();

        try scanDirectoryForUnusedSuppression(allocator, dir, "src", &violations);

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

    // Files in backend/llvm that are copies from Zig's stdlib and should be excluded
    const stdlib_copies = [_][]const u8{
        "backend/llvm/Builder.zig",
        "backend/llvm/ir.zig",
        "backend/llvm/bitcode_writer.zig",
        "backend/llvm/BitcodeReader.zig",
    };

    fn isStdlibCopy(path: []const u8) bool {
        for (stdlib_copies) |excluded| {
            if (std.mem.endsWith(u8, path, excluded)) return true;
        }
        return false;
    }

    fn scanDirectoryForUnusedSuppression(
        allocator: std.mem.Allocator,
        dir: std.fs.Dir,
        path_prefix: []const u8,
        violations: *std.ArrayList(Violation),
    ) !void {
        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".zig")) continue;

            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path_prefix, entry.path });

            // Skip Zig stdlib copies in backend/llvm
            if (isStdlibCopy(full_path)) continue;

            const file = dir.openFile(entry.path, .{}) catch continue;
            defer file.close();

            const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch continue;
            defer allocator.free(content);

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
        "src/eval/StackValue.zig",
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
            if (std.mem.indexOf(u8, line, pattern) != null) return true;
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

    fn scanFile(allocator: std.mem.Allocator, file_path: []const u8, violations: *std.ArrayList(Violation)) !void {
        const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
            std.debug.print("Warning: Failed to open {s}: {}\n", .{ file_path, err });
            return;
        };
        defer file.close();

        const content = file.readToEndAlloc(allocator, 50 * 1024 * 1024) catch |err| {
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
                    const has_panic = std.mem.indexOf(u8, line, "@panic(") != null;
                    // Check for std.debug.panic usage
                    const has_debug_panic = std.mem.indexOf(u8, line, "std.debug.panic") != null;

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

        // Scan individual files
        for (scan_files) |file_path| {
            try scanFile(allocator, file_path, &violations);
        }

        // Scan directories
        for (scan_dirs) |dir_path| {
            var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
                std.debug.print("Warning: Failed to open directory {s}: {}\n", .{ dir_path, err });
                continue;
            };
            defer dir.close();

            var iter = dir.iterate();
            while (try iter.next()) |entry| {
                if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".zig")) {
                    if (!isExcludedFile(entry.name)) {
                        const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
                        defer allocator.free(full_path);
                        try scanFile(allocator, full_path, &violations);
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
                \\  In StackValue.zig and builtins, use roc_ops.crash():
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
        const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
            return step.fail("Failed to open {s}: {}", .{ file_path, err });
        };
        defer file.close();

        const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch |err| {
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
                    if (std.mem.indexOf(u8, trimmed, pattern) != null) {
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
                \\  Access stdout/stderr through the CliContext:
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

    /// Minimum required coverage percentage. Build fails if coverage drops below this.
    /// This threshold should be gradually increased as more tests are added.
    ///
    /// Coverage is supported on:
    /// - macOS (ARM64 and x86_64): Uses libdwarf for DWARF parsing
    /// - Linux ARM64: Uses libdw (elfutils) for DWARF parsing
    ///
    /// Coverage does NOT work on Linux x86_64 due to a Zig 0.15.2 compiler bug that
    /// generates invalid DWARF .debug_line sections. libdw fails with "invalid
    /// .debug_line section" when parsing user code compilation units, while stdlib
    /// CUs parse successfully. This causes kcov to find only stdlib files, not user
    /// source files. ARM64 Zig generates valid DWARF, so coverage works there.
    /// See: https://github.com/roc-lang/roc/pull/8864 for investigation details.
    const MIN_COVERAGE_PERCENT: f64 = 28.0;

    fn create(b: *std.Build, coverage_dir: []const u8) *CoverageSummaryStep {
        const self = b.allocator.create(CoverageSummaryStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "coverage-summary",
                .owner = b,
                .makeFn = make,
            }),
            .coverage_dir = coverage_dir,
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
        const json_path = try std.fmt.allocPrint(allocator, "{s}/parse_unit_coverage/coverage.json", .{self.coverage_dir});
        defer allocator.free(json_path);

        const json_file = std.fs.cwd().openFile(json_path, .{}) catch |err| {
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
        defer json_file.close();

        const json_content = try json_file.readToEndAlloc(allocator, 10 * 1024 * 1024);
        defer allocator.free(json_content);

        // Parse and summarize coverage
        const result = try parseCoverageJson(allocator, json_content);

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
        if (result.percent < MIN_COVERAGE_PERCENT) {
            std.debug.print("\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            std.debug.print("COVERAGE CHECK FAILED\n", .{});
            std.debug.print("=" ** 60 ++ "\n\n", .{});
            std.debug.print("Parser coverage is {d:.2}%, minimum required is {d:.2}%\n", .{ result.percent, MIN_COVERAGE_PERCENT });
            std.debug.print("Add more tests to improve coverage before merging.\n\n", .{});
            std.debug.print("=" ** 60 ++ "\n", .{});
            return step.fail("Parser coverage {d:.2}% is below minimum {d:.2}%", .{ result.percent, MIN_COVERAGE_PERCENT });
        }
    }

    const CoverageResult = struct {
        percent: f64,
        total_lines: u64,
    };

    fn parseCoverageJson(allocator: std.mem.Allocator, json_content: []const u8) !CoverageResult {
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
                    if (std.mem.indexOf(u8, filename, "src/parse") == null) continue;

                    // Skip test files
                    if (std.mem.indexOf(u8, filename, "/test/") != null) continue;

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
        std.debug.print("PARSER CODE COVERAGE SUMMARY\n", .{});
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
        std.debug.print("Full HTML report: kcov-output/parser/index.html\n", .{});
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
    var fx_dir = try std.fs.cwd().openDir("test/fx", .{ .iterate = true });
    defer fx_dir.close();

    var roc_files = std.ArrayList([]const u8).empty;
    defer {
        for (roc_files.items) |file| {
            allocator.free(file);
        }
        roc_files.deinit(allocator);
    }

    var dir_iter = fx_dir.iterate();
    while (try dir_iter.next()) |entry| {
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
        const test_file_contents = std.fs.cwd().readFileAlloc(allocator, test_file_path, 1024 * 1024) catch |err| {
            std.debug.print("Warning: Could not read {s}: {}\n", .{ test_file_path, err });
            continue;
        };
        defer allocator.free(test_file_contents);

        var line_iter = std.mem.splitScalar(u8, test_file_contents, '\n');
        while (line_iter.next()) |line| {
            // Look for patterns like "test/fx/filename.roc"
            var search_start: usize = 0;
            while (std.mem.indexOfPos(u8, line, search_start, "test/fx/")) |idx| {
                const rest_of_line = line[idx..];
                // Find the end of the filename
                if (std.mem.indexOf(u8, rest_of_line, ".roc")) |roc_pos| {
                    const full_path = rest_of_line[0 .. roc_pos + 4]; // Include ".roc"
                    // Extract just the filename (after "test/fx/")
                    const filename = full_path["test/fx/".len..];
                    // Only count files in test/fx (not subdirectories like test/fx/subdir/)
                    if (std.mem.indexOf(u8, filename, "/") == null) {
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

const MiniCiStep = struct {
    step: Step,

    fn create(b: *std.Build) *MiniCiStep {
        const self = b.allocator.create(MiniCiStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "minici-inner",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;

        // Run the sequence of `zig build` commands that make up the
        // mini CI pipeline.
        try runSubBuild(step, "fmt", "zig build fmt");
        try runZigLints(step);
        try runTidy(step);
        try checkTestWiring(step);
        try runSubBuild(step, null, "zig build");
        try checkBuiltinRocFormatting(step);
        try runSubBuild(step, "snapshot", "zig build snapshot");
        try checkSnapshotChanges(step);
        try checkFxPlatformTestCoverage(step);
        try runSubBuild(step, "test", "zig build test");
        try runSubBuild(step, "test-playground", "zig build test-playground");
        try runSubBuild(step, "test-serialization-sizes", "zig build test-serialization-sizes");
        try runSubBuild(step, "test-cli", "zig build test-cli");
        try runSubBuild(step, "coverage", "zig build coverage");
    }

    fn runZigLints(step: *Step) !void {
        const b = step.owner;
        std.debug.print("---- minici: running zig lints ----\n", .{});

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        try child_argv.append(b.allocator, b.graph.zig_exe);
        try child_argv.append(b.allocator, "run");
        try child_argv.append(b.allocator, "ci/zig_lints.zig");

        var child = std.process.Child.init(child_argv.items, b.allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    return step.fail("Zig lints failed. Run 'zig run ci/zig_lints.zig' to see details.", .{});
                }
            },
            else => {
                return step.fail("zig run ci/zig_lints.zig terminated abnormally", .{});
            },
        }
    }

    fn runTidy(step: *Step) !void {
        const b = step.owner;
        std.debug.print("---- minici: running tidy checks ----\n", .{});

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        try child_argv.append(b.allocator, b.graph.zig_exe);
        try child_argv.append(b.allocator, "run");
        try child_argv.append(b.allocator, "ci/tidy.zig");

        var child = std.process.Child.init(child_argv.items, b.allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    return step.fail("Tidy checks failed. Run 'zig run ci/tidy.zig' to see details.", .{});
                }
            },
            else => {
                return step.fail("zig run ci/tidy.zig terminated abnormally", .{});
            },
        }
    }

    fn checkBuiltinRocFormatting(step: *Step) !void {
        const b = step.owner;
        std.debug.print("---- minici: checking Builtin.roc formatting ----\n", .{});

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        try child_argv.append(b.allocator, "./zig-out/bin/roc");
        try child_argv.append(b.allocator, "fmt");
        try child_argv.append(b.allocator, "--check");
        try child_argv.append(b.allocator, "src/build/roc/Builtin.roc");

        var child = std.process.Child.init(child_argv.items, b.allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    return step.fail(
                        "src/build/roc/Builtin.roc is not formatted. " ++
                            "Run 'zig build run -- fmt src/build/roc/Builtin.roc' to format it.",
                        .{},
                    );
                }
            },
            else => {
                return step.fail("roc fmt --check terminated abnormally", .{});
            },
        }
    }

    fn checkSnapshotChanges(step: *Step) !void {
        const b = step.owner;
        std.debug.print("---- minici: checking for snapshot changes ----\n", .{});

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        try child_argv.append(b.allocator, "git");
        try child_argv.append(b.allocator, "diff");
        try child_argv.append(b.allocator, "--exit-code");
        try child_argv.append(b.allocator, "test/snapshots");

        var child = std.process.Child.init(child_argv.items, b.allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    return step.fail(
                        "Snapshots in 'test/snapshots' have changed. " ++
                            "Run 'zig build snapshot' locally, review the updates, and commit the changes.",
                        .{},
                    );
                }
            },
            else => {
                return step.fail("git diff terminated abnormally", .{});
            },
        }
    }

    fn runSubBuild(
        step: *Step,
        step_name: ?[]const u8,
        display: []const u8,
    ) !void {
        const b = step.owner;
        std.debug.print("---- minici: running `{s}` ----\n", .{display});

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        // Build a clean zig build command for the requested step.
        try child_argv.append(b.allocator, b.graph.zig_exe); // zig executable
        try child_argv.append(b.allocator, "build");

        if (step_name) |name| {
            try child_argv.append(b.allocator, name);
        }

        var child = std.process.Child.init(child_argv.items, b.allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    return step.fail("`{s}` failed with exit code {d}", .{ display, code });
                }
            },
            else => {
                return step.fail("`{s}` terminated abnormally", .{display});
            },
        }
    }

    fn checkTestWiring(step: *Step) !void {
        const b = step.owner;
        std.debug.print("---- minici: checking test wiring ----\n", .{});

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        try child_argv.append(b.allocator, b.graph.zig_exe);
        try child_argv.append(b.allocator, "run");
        try child_argv.append(b.allocator, "ci/check_test_wiring.zig");

        var child = std.process.Child.init(child_argv.items, b.allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    return step.fail(
                        "Test wiring check failed. Run 'zig run ci/check_test_wiring.zig' to see details.",
                        .{},
                    );
                }
            },
            else => {
                return step.fail("zig run ci/check_test_wiring.zig terminated abnormally", .{});
            },
        }
    }
};

const TidyStep = struct {
    step: Step,

    fn create(b: *std.Build) *TidyStep {
        const self = b.allocator.create(TidyStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "tidy-inner",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, _: Step.MakeOptions) !void {
        const b = step.owner;
        std.debug.print("---- tidy: running code tidiness checks ----\n", .{});

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(b.allocator);

        try child_argv.append(b.allocator, b.graph.zig_exe);
        try child_argv.append(b.allocator, "run");
        try child_argv.append(b.allocator, "ci/tidy.zig");

        var child = std.process.Child.init(child_argv.items, b.allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    return step.fail("Tidy checks failed. Run 'zig run ci/tidy.zig' to see details.", .{});
                }
            },
            else => {
                return step.fail("zig run ci/tidy.zig terminated abnormally", .{});
            },
        }
    }
};

fn createAndRunBuiltinCompiler(
    b: *std.Build,
    roc_modules: modules.RocModules,
    flag_enable_tracy: ?[]const u8,
    roc_files: []const []const u8,
) *Step.Run {
    // Build and run the compiler
    const builtin_compiler_exe = b.addExecutable(.{
        .name = "builtin_compiler",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/builtin_compiler/main.zig"),
            .target = b.graph.host, // this runs at build time on the *host* machine!
            .optimize = .Debug, // No need to optimize - only compiles builtin modules
            // Note: libc linking is handled by add_tracy below (required when tracy is enabled)
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

    return run_builtin_compiler;
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
        }),
    });
    configureBackend(lib, target);
    lib.root_module.addImport("builtins", roc_modules.builtins);
    lib.root_module.addImport("build_options", roc_modules.build_options);
    // Don't bundle compiler-rt in host libraries - roc_shim provides it
    // Bundling it here causes duplicate symbol errors on Windows
    lib.bundle_compiler_rt = false;

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
    const lib = createTestPlatformHostLib(
        b,
        b.fmt("test_platform_{s}_host_{s}", .{ platform_dir, target_name }),
        b.pathJoin(&.{ "test", platform_dir, "platform/host.zig" }),
        target,
        optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
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

        const file = std.fs.cwd().openFile(self.archive_path, .{ .mode = .read_write }) catch |err| {
            std.debug.print("Warning: Could not open archive {s}: {s}\n", .{ self.archive_path, @errorName(err) });
            return;
        };
        defer file.close();

        const stat = try file.stat();
        var file_size = stat.size;

        // AR format requires archives to end on an even byte boundary.
        // If file size is odd, append a newline padding byte.
        // This fixes Zig bug https://codeberg.org/ziglang/zig/issues/30572
        // where Zig's archiver doesn't add required padding after odd-sized members.
        if (file_size % 2 == 1) {
            try file.seekTo(file_size);
            try file.writeAll("\n");
            file_size += 1;
        }

        // Parse the archive to verify member offsets are valid.
        // This catches cases where lld would fail with "truncated or malformed archive".
        try file.seekTo(0);
        var header_buf: [8]u8 = undefined;
        _ = try file.read(&header_buf);
        if (!std.mem.eql(u8, &header_buf, "!<arch>\n")) {
            std.debug.print("Warning: Invalid archive magic in {s}\n", .{self.archive_path});
            return;
        }

        var offset: u64 = 8; // After magic
        while (offset + 60 <= file_size) {
            try file.seekTo(offset + 48); // Seek to size field (offset 48 within 60-byte header)
            var size_buf: [10]u8 = undefined;
            _ = try file.read(&size_buf);

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
                try file.seekTo(file_size);
                const padding = "\n\n"; // At most 1 byte needed, but be safe
                try file.writeAll(padding[0..@min(missing, 2)]);
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

        const allocator = step.owner.allocator;

        // Get the cache directory path using the same logic as cache_config.zig
        const cache_dir = getCacheDir(allocator) catch |err| {
            std.debug.print("Warning: Could not determine cache directory: {s}\n", .{@errorName(err)});
            return;
        };
        defer allocator.free(cache_dir);

        // Check if cache directory exists before trying to delete
        std.fs.cwd().access(cache_dir, .{}) catch {
            // Cache doesn't exist, nothing to do
            std.debug.print("Roc cache not found (nothing to clear)\n", .{});
            return;
        };

        // Try to delete the cache directory
        std.fs.cwd().deleteTree(cache_dir) catch |err| {
            std.debug.print("Warning: Could not clear cache at {s}: {s}\n", .{ cache_dir, @errorName(err) });
            return;
        };

        std.debug.print("Cleared roc cache at {s}\n", .{cache_dir});
    }

    /// Get the Roc cache directory path (matches cache_config.zig logic)
    fn getCacheDir(allocator: std.mem.Allocator) ![]u8 {
        const cache_dir_name = switch (builtin.os.tag) {
            .windows => "Roc",
            else => "roc",
        };

        // Respect XDG_CACHE_HOME if set
        if (std.process.getEnvVarOwned(allocator, "XDG_CACHE_HOME")) |xdg_cache| {
            defer allocator.free(xdg_cache);
            return std.fs.path.join(allocator, &[_][]const u8{ xdg_cache, cache_dir_name });
        } else |_| {
            // Fall back to platform defaults
            const home_env = switch (builtin.os.tag) {
                .windows => "APPDATA",
                else => "HOME",
            };

            const home_dir = std.process.getEnvVarOwned(allocator, home_env) catch {
                return error.NoHomeDirectory;
            };
            defer allocator.free(home_dir);

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
    test_platforms_step: *Step,
    strip: bool,
    omit_frame_pointer: ?bool,
    platform_filter: ?[]const u8,
) void {
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

    // Cross-compile for musl targets (glibc not needed for test-platforms step)
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

    // Build the wasm test platform host for wasm32-freestanding
    {
        const wasm_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none });
        const copy_step = buildAndCopyTestPlatformHostLib(
            b,
            "wasm",
            wasm_target,
            "wasm32",
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
        );
        clear_cache_step.dependOn(copy_step);
    }

    b.getInstallStep().dependOn(clear_cache_step);
    test_platforms_step.dependOn(clear_cache_step);
}

pub fn build(b: *std.Build) void {
    // build steps
    const run_step = b.step("run", "Build and run the roc cli");
    const roc_step = b.step("roc", "Build the roc compiler without running it");
    const test_step = b.step("test", "Run all tests included in src/tests.zig");
    const minici_step = b.step("minici", "Run a subset of CI build and test steps");
    const tidy_step = b.step("tidy", "Run code tidiness checks (control chars, line length, etc.)");
    const checkfx_step = b.step("checkfx", "Check that every .roc file in test/fx has a corresponding test");
    const fmt_step = b.step("fmt", "Format all zig code");
    const check_fmt_step = b.step("check-fmt", "Check formatting of all zig code");
    const snapshot_step = b.step("snapshot", "Run the snapshot tool to update snapshot files");
    const playground_step = b.step("playground", "Build the WASM playground");
    const playground_test_step = b.step("test-playground", "Build the integration test suite for the WASM playground");
    const serialization_size_step = b.step("test-serialization-sizes", "Verify Serialized types have platform-independent sizes");
    const wasm_static_lib_test_step = b.step("test-wasm-static-lib", "Test WASM static library builds with bytebox");
    const test_cli_step = b.step("test-cli", "Test the roc CLI by running test programs");
    const test_cli_dev_step = b.step("test-cli-dev", "Test the roc CLI with --backend=dev (informational, not in CI)");
    const test_platforms_step = b.step("test-platforms", "Build test platform host libraries");
    const coverage_step = b.step("coverage", "Run parser tests with kcov code coverage");
    const release_step = b.step("release", "Build optimized release binary for distribution");

    // general configuration
    const target = blk: {
        var default_target_query: std.Target.Query = .{
            .abi = if (builtin.target.os.tag == .linux) .musl else null,
        };

        // Use baseline x86_64 CPU for Valgrind compatibility on CI (Valgrind 3.18.1 doesn't support AVX-512)
        const is_ci = std.process.getEnvVarOwned(b.allocator, "CI") catch null;
        if (is_ci != null and builtin.target.cpu.arch == .x86_64 and builtin.target.os.tag == .linux) {
            default_target_query.cpu_model = .{ .explicit = &std.Target.x86.cpu.x86_64 };
        }

        break :blk b.standardTargetOptions(.{ .default_target = default_target_query });
    };
    const optimize = b.standardOptimizeOption(.{});
    const strip_flag = b.option(bool, "strip", "Omit debug information");
    const no_bin = b.option(bool, "no-bin", "Skip emitting binaries (important for fast incremental compilation)") orelse false;
    const trace_eval = b.option(bool, "trace-eval", "Enable detailed evaluation tracing for debugging") orelse (optimize == .Debug);
    const trace_refcount = b.option(bool, "trace-refcount", "Enable detailed refcount tracing for debugging memory issues") orelse false;
    const trace_modules = b.option(bool, "trace-modules", "Enable module compilation and import resolution tracing") orelse false;
    const platform_filter = b.option([]const u8, "platform", "Filter which test platform to build (e.g., fx, str, int, fx-open)");
    const trace_build = b.option(bool, "trace-build", "Enable detailed build pipeline tracing") orelse false;

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
    build_options.addOption([]const u8, "compiler_version", getCompilerVersion(b, optimize));
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
    const run_builtin_compiler = createAndRunBuiltinCompiler(b, roc_modules, flag_enable_tracy, &.{builtin_roc_path});
    write_compiled_builtins.step.dependOn(&run_builtin_compiler.step);

    // Copy Builtin.bin from zig-out/builtins/
    _ = write_compiled_builtins.addCopyFile(
        .{ .cwd_relative = "zig-out/builtins/Builtin.bin" },
        "Builtin.bin",
    );

    // Copy the source Builtin.roc file for embedding
    _ = write_compiled_builtins.addCopyFile(
        b.path(builtin_roc_path),
        "Builtin.roc",
    );

    // Copy builtin_indices.bin
    _ = write_compiled_builtins.addCopyFile(
        .{ .cwd_relative = "zig-out/builtins/builtin_indices.bin" },
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

    roc_modules.repl.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.compile.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.eval.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.lsp.addImport("compiled_builtins", compiled_builtins_module);

    // Setup test platform host libraries
    setupTestPlatforms(b, target, optimize, roc_modules, test_platforms_step, strip, omit_frame_pointer, platform_filter);

    const roc_exe = addMainExe(b, roc_modules, target, optimize, strip, omit_frame_pointer, use_system_llvm, user_llvm_path, flag_enable_tracy, zstd, compiled_builtins_module, write_compiled_builtins, flag_enable_tracy) orelse return;
    roc_modules.addAll(roc_exe);
    install_and_run(b, no_bin, roc_exe, roc_step, run_step, run_args);

    // Clear the Roc cache when building the compiler to ensure stale cached artifacts aren't used
    const clear_cache_step = createClearCacheStep(b);
    roc_step.dependOn(clear_cache_step);
    b.getInstallStep().dependOn(clear_cache_step);

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
            null, // No tracy
        );
        if (release_exe) |exe| {
            roc_modules.addAll(exe);
            exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
            exe.step.dependOn(&write_compiled_builtins.step);
            const install = b.addInstallArtifact(exe, .{});
            release_step.dependOn(&install.step);
        }
    }

    // Unified test platform runner (replaces fx_cross_runner and int_cross_runner)
    const test_runner_exe = b.addExecutable(.{
        .name = "test_runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/test/test_runner.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(test_runner_exe);

    // Store glue test step reference so we can add glue host dependency later
    var run_glue_test_step: ?*std.Build.Step = null;

    // CLI integration tests - run actual roc programs like CI does
    // These tests can run in parallel since each build uses content-hashed shim files
    if (!no_bin) {
        const install = b.addInstallArtifact(roc_exe, .{});
        const install_runner = b.addInstallArtifact(test_runner_exe, .{});

        // Test int platform (native mode only for now)
        const run_int_tests = b.addRunArtifact(test_runner_exe);
        run_int_tests.addArg("zig-out/bin/roc");
        run_int_tests.addArg("int");
        run_int_tests.addArg("--mode=native");
        run_int_tests.step.dependOn(&install.step);
        run_int_tests.step.dependOn(&install_runner.step);
        run_int_tests.step.dependOn(test_platforms_step);
        test_cli_step.dependOn(&run_int_tests.step);

        // Test str platform (native mode only for now)
        const run_str_tests = b.addRunArtifact(test_runner_exe);
        run_str_tests.addArg("zig-out/bin/roc");
        run_str_tests.addArg("str");
        run_str_tests.addArg("--mode=native");
        run_str_tests.step.dependOn(&install.step);
        run_str_tests.step.dependOn(&install_runner.step);
        run_str_tests.step.dependOn(test_platforms_step);
        test_cli_step.dependOn(&run_str_tests.step);

        // Roc subcommands integration test
        const roc_subcommands_test = b.addTest(.{
            .name = "roc_subcommands_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/test/roc_subcommands.zig"),
                .target = target,
                .optimize = optimize,
            }),
            .filters = test_filters,
        });

        const run_roc_subcommands_test = b.addRunArtifact(roc_subcommands_test);
        if (run_args.len != 0) {
            run_roc_subcommands_test.addArgs(run_args);
        }
        run_roc_subcommands_test.step.dependOn(&install.step);
        run_roc_subcommands_test.step.dependOn(test_platforms_step);
        test_cli_step.dependOn(&run_roc_subcommands_test.step);

        // Glue command integration test
        const glue_test = b.addTest(.{
            .name = "glue_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/test/glue_test.zig"),
                .target = target,
                .optimize = optimize,
            }),
            .filters = test_filters,
        });

        const run_glue_test = b.addRunArtifact(glue_test);
        if (run_args.len != 0) {
            run_glue_test.addArgs(run_args);
        }
        run_glue_test.step.dependOn(&install.step);
        run_glue_test_step = &run_glue_test.step;
        test_cli_step.dependOn(&run_glue_test.step);
        // Tests fx platform with --backend=dev to track dev backend progress
        const run_fx_dev_tests = b.addRunArtifact(test_runner_exe);
        run_fx_dev_tests.addArg("zig-out/bin/roc");
        run_fx_dev_tests.addArg("fx");
        run_fx_dev_tests.addArg("--mode=native");
        run_fx_dev_tests.addArg("--backend=dev");
        run_fx_dev_tests.step.dependOn(&install.step);
        run_fx_dev_tests.step.dependOn(&install_runner.step);
        run_fx_dev_tests.step.dependOn(test_platforms_step);
        test_cli_dev_step.dependOn(&run_fx_dev_tests.step);
    }

    // Manual rebuild command: zig build rebuild-builtins
    // Use this after making compiler changes to ensure those changes are reflected in builtins
    const rebuild_builtins_step = b.step(
        "rebuild-builtins",
        "Force rebuild of all builtin modules (*.roc -> *.bin)",
    );

    // Clean zig-out/ to ensure a fresh rebuild of builtins
    // Note: We don't delete .zig-cache because it contains build options needed during compilation.
    const clean_out_step = b.addRemoveDirTree(b.path("zig-out"));

    // Also clear the roc cache to avoid stale cached modules with old struct layouts
    const clear_roc_cache_step = createClearCacheStep(b);

    // Discover .roc files again for the rebuild command
    const roc_files_force = discoverBuiltinRocFiles(b) catch |err| {
        std.debug.print("Failed to discover .roc files for rebuild: {}\n", .{err});
        return;
    };

    const run_builtin_compiler_force = createAndRunBuiltinCompiler(b, roc_modules, flag_enable_tracy, roc_files_force);
    run_builtin_compiler_force.step.dependOn(&clean_out_step.step);
    run_builtin_compiler_force.step.dependOn(clear_roc_cache_step);
    rebuild_builtins_step.dependOn(&run_builtin_compiler_force.step);

    // Add the compiled builtins module to roc exe and make it depend on the builtins being ready
    roc_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    roc_exe.step.dependOn(&write_compiled_builtins.step);

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

    // Add LLVM support to snapshot tool for dual-mode testing
    const llvm_paths = llvmPaths(b, target, use_system_llvm, user_llvm_path) orelse return;
    snapshot_exe.addLibraryPath(.{ .cwd_relative = llvm_paths.lib });
    snapshot_exe.addIncludePath(.{ .cwd_relative = llvm_paths.include });
    try addStaticLlvmOptionsToModule(snapshot_exe.root_module);
    // Add llvm_compile module for LLVM compilation pipeline
    snapshot_exe.root_module.addAnonymousImport("llvm_compile", .{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
        .imports = &.{.{ .name = "builtins", .module = roc_modules.builtins }},
    });

    add_tracy(b, roc_modules.build_options, snapshot_exe, target, true, flag_enable_tracy);
    install_and_run(b, no_bin, snapshot_exe, snapshot_step, snapshot_step, run_args);

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
    playground_step.dependOn(&playground_install.step);

    const bytebox = b.dependency("bytebox", .{
        .target = target,
        .optimize = optimize,
    });

    // Build playground integration tests - now enabled for all optimization modes
    const playground_test_install = blk: {
        const playground_integration_test_exe = b.addExecutable(.{
            .name = "playground_integration_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/playground-integration/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(playground_integration_test_exe, target);
        playground_integration_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
        playground_integration_test_exe.root_module.addImport("build_options", build_options.createModule());
        roc_modules.addAll(playground_integration_test_exe);

        const install = b.addInstallArtifact(playground_integration_test_exe, .{});
        // Ensure playground WASM is built before running the integration test
        install.step.dependOn(&playground_install.step);
        playground_test_step.dependOn(&install.step);

        const run_playground_test = b.addRunArtifact(playground_integration_test_exe);
        if (run_args.len != 0) {
            run_playground_test.addArgs(run_args);
        }
        run_playground_test.step.dependOn(&install.step);
        playground_test_step.dependOn(&run_playground_test.step);

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
        serialization_size_step.dependOn(&size_check_native.step);
        serialization_size_step.dependOn(&size_check_wasm32.step);
        serialization_size_step.dependOn(&run_native.step);
    }

    // Build WASM static library test runner with bytebox
    // This test requires the WASM file to be built separately via `roc build test/wasm/app.roc --target=wasm32`
    {
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
        wasm_static_lib_test_step.dependOn(&install.step);

        const run_wasm_test = b.addRunArtifact(wasm_test_exe);
        if (run_args.len != 0) {
            run_wasm_test.addArgs(run_args);
        }
        run_wasm_test.step.dependOn(&install.step);
        wasm_static_lib_test_step.dependOn(&run_wasm_test.step);
    }

    // Check fx platform test coverage convenience step
    const checkfx_inner = CheckFxStep.create(b);
    checkfx_step.dependOn(&checkfx_inner.step);

    // Mini CI convenience step: runs a sequence of common build and test commands in order.
    const minici_inner = MiniCiStep.create(b);
    minici_step.dependOn(&minici_inner.step);

    // Tidy step: run code tidiness checks
    const tidy_inner = TidyStep.create(b);
    tidy_step.dependOn(&tidy_inner.step);

    // Create and add module tests
    const module_tests_result = roc_modules.createModuleTests(b, target, optimize, zstd, test_filters);
    const tests_summary = TestsSummaryStep.create(b, test_filters, module_tests_result.forced_passes);
    for (module_tests_result.tests) |module_test| {
        // Add compiled builtins to check, repl, eval, compile, and lsp module tests
        if (std.mem.eql(u8, module_test.test_step.name, "check") or std.mem.eql(u8, module_test.test_step.name, "repl") or std.mem.eql(u8, module_test.test_step.name, "eval") or std.mem.eql(u8, module_test.test_step.name, "compile") or std.mem.eql(u8, module_test.test_step.name, "lsp") or std.mem.eql(u8, module_test.test_step.name, "mir")) {
            module_test.test_step.root_module.addImport("compiled_builtins", compiled_builtins_module);
            module_test.test_step.step.dependOn(&write_compiled_builtins.step);
        }

        // Add bytebox to eval tests for wasm backend testing
        if (std.mem.eql(u8, module_test.test_step.name, "eval")) {
            module_test.test_step.root_module.addImport("bytebox", bytebox.module("bytebox"));
        }

        if (run_args.len != 0) {
            module_test.run_step.addArgs(run_args);
        }

        // Create individual test step for this module
        const test_exe_name = module_test.test_step.name;
        const step_name = b.fmt("test-{s}", .{test_exe_name});
        const individual_test_step = b.step(step_name, b.fmt("Run {s} tests only", .{test_exe_name}));

        // Create run step that accepts command line args (including --test-filter)
        const individual_run = b.addRunArtifact(module_test.test_step);
        if (run_args.len != 0) {
            individual_run.addArgs(run_args);
        }
        individual_test_step.dependOn(&individual_run.step);

        b.default_step.dependOn(&module_test.test_step.step);
        tests_summary.addRun(&module_test.run_step.step);
    }

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

        // Add LLVM support for dual-mode testing
        const llvm_paths_test = llvmPaths(b, target, use_system_llvm, user_llvm_path) orelse return;
        snapshot_test.addLibraryPath(.{ .cwd_relative = llvm_paths_test.lib });
        snapshot_test.addIncludePath(.{ .cwd_relative = llvm_paths_test.include });
        try addStaticLlvmOptionsToModule(snapshot_test.root_module);
        snapshot_test.root_module.addAnonymousImport("llvm_compile", .{
            .root_source_file = b.path("src/llvm_compile/mod.zig"),
            .imports = &.{.{ .name = "builtins", .module = roc_modules.builtins }},
        });

        add_tracy(b, roc_modules.build_options, snapshot_test, target, true, flag_enable_tracy);

        const run_snapshot_test = b.addRunArtifact(snapshot_test);
        if (run_args.len != 0) {
            run_snapshot_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_snapshot_test.step);
    }

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
        cli_test.linkLibrary(zstd.artifact("zstd"));
        add_tracy(b, roc_modules.build_options, cli_test, target, false, flag_enable_tracy);
        cli_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        cli_test.step.dependOn(&write_compiled_builtins.step);

        const run_cli_test = b.addRunArtifact(cli_test);
        if (run_args.len != 0) {
            run_cli_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_cli_test.step);
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
            watch_test.linkFramework("CoreFoundation");
            watch_test.linkFramework("CoreServices");
        } else if (target.result.os.tag == .windows) {
            watch_test.linkSystemLibrary("kernel32");
        }

        const run_watch_test = b.addRunArtifact(watch_test);
        if (run_args.len != 0) {
            run_watch_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_watch_test.step);
    }

    // Add check for forbidden patterns in type checker code
    const check_patterns = CheckTypeCheckerPatternsStep.create(b);
    test_step.dependOn(&check_patterns.step);

    // Add check for @enumFromInt(0) usage
    const check_enum_from_int = CheckEnumFromIntZeroStep.create(b);
    test_step.dependOn(&check_enum_from_int.step);

    // Add check for unused variable suppression patterns
    const check_unused = CheckUnusedSuppressionStep.create(b);
    test_step.dependOn(&check_unused.step);

    // Check for @panic and std.debug.panic in interpreter and builtins
    const check_panic = CheckPanicStep.create(b);
    test_step.dependOn(&check_panic.step);

    // Add check for global stdio usage in CLI code
    const check_cli_stdio = CheckCliGlobalStdioStep.create(b);
    test_step.dependOn(&check_cli_stdio.step);

    test_step.dependOn(&tests_summary.step);

    b.default_step.dependOn(playground_step);
    {
        const install = playground_test_install;
        b.default_step.dependOn(&install.step);
    }

    // Fmt zig code.
    const fmt_paths = .{ "src", "build.zig" };
    const fmt = b.addFmt(.{ .paths = &fmt_paths });
    fmt_step.dependOn(&fmt.step);

    const check_fmt = b.addFmt(.{ .paths = &fmt_paths, .check = true });
    check_fmt_step.dependOn(&check_fmt.step);

    // Parser code coverage with kcov
    // Only supported on Linux ARM64 and macOS (kcov doesn't work on Windows)
    // Linux x86_64 is NOT supported due to Zig 0.15.2 generating invalid DWARF .debug_line
    // sections that cause kcov to fail (see CoverageSummaryStep comments for details)
    const is_linux_x86_64 = target.result.os.tag == .linux and target.result.cpu.arch == .x86_64;
    const is_coverage_supported = (target.result.os.tag == .linux or target.result.os.tag == .macos) and !is_linux_x86_64;
    if (is_coverage_supported and isNativeishOrMusl(target)) {
        // Get the kcov dependency and build it from source
        // lazyDependency returns null on first pass; Zig re-runs build() after fetching
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

            // Make coverage step depend on the build step
            coverage_step.dependOn(build_cov_tests);

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
            const summary_step = CoverageSummaryStep.create(b, "kcov-output/parser");
            summary_step.step.dependOn(&run_parse_coverage.step);

            // Cross-compile for Windows to verify comptime branches compile
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
            coverage_step.dependOn(&windows_parse_build.step);

            // Add explicit dependencies on install steps to coverage_step itself
            // to work around Zig 0.15.2 lazy dependency issues
            coverage_step.dependOn(&install_parse_test.step);
            coverage_step.dependOn(&install_kcov.step);

            // Hook up coverage_step to the summary step
            coverage_step.dependOn(&summary_step.step);
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
                    std.debug.print("kcov is only supported on Linux and macOS.\n", .{});
                    std.debug.print("Current platform: {s}\n\n", .{@tagName(builtin.target.os.tag)});
                    std.debug.print("=" ** 60 ++ "\n", .{});
                }
            }.make,
        });
        coverage_step.dependOn(unsupported_step);
    }

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

        const fx_platform_test = b.addTest(.{
            .name = "fx_platform_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/test/fx_platform_test.zig"),
                .target = target,
                .optimize = optimize,
            }),
            .filters = test_filters,
        });

        const run_fx_platform_test = b.addRunArtifact(fx_platform_test);
        if (run_args.len != 0) {
            run_fx_platform_test.addArgs(run_args);
        }
        // Ensure host library is copied AND fixed before running the test
        run_fx_platform_test.step.dependOn(final_fx_host_step);
        // Ensure roc binary is built before running the test (tests invoke roc CLI)
        run_fx_platform_test.step.dependOn(roc_step);
        tests_summary.addRun(&run_fx_platform_test.step);
    }

    // Build glue platform host at runtime for the native platform.
    if (run_glue_test_step) |glue_test_step| {
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
                );

                // Add compiler modules to glue platform host for type extraction
                glue_platform_host_lib.root_module.addImport("base", roc_modules.base);
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
                std.fs.cwd().makePath(dir_path) catch {};

                copy_glue_host.addCopyFileToSource(glue_platform_host_lib.getEmittedBin(), target_path);

                // Apply archive padding fix for non-Windows targets
                const final_step: *Step = if (target.result.os.tag != .windows) blk: {
                    const fix_target = FixArchivePaddingStep.create(b, target_path);
                    fix_target.step.dependOn(&copy_glue_host.step);
                    break :blk &fix_target.step;
                } else &copy_glue_host.step;

                glue_test_step.dependOn(final_step);
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
    const builtin_roc_path = try b.build_root.join(b.allocator, &.{ "src", "build", "roc" });
    var builtin_roc_dir = try std.fs.openDirAbsolute(builtin_roc_path, .{ .iterate = true });
    defer builtin_roc_dir.close();

    var roc_files = std.ArrayList([]const u8).empty;
    errdefer roc_files.deinit(b.allocator);

    var iter = builtin_roc_dir.iterate();
    while (try iter.next()) |entry| {
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
    const repro_step = b.step(name_repro, b.fmt("run fuzz reproduction for {s}", .{name}));
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

    install_and_run(b, no_bin, repro_exe, repro_step, repro_step, run_args);

    if (fuzz and build_afl and !no_bin) {
        const fuzz_step = b.step(name_exe, b.fmt("Generate fuzz executable for {s}", .{name}));
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
    configureBackend(exe, target);

    // Build str and int test platform host libraries for native target
    // (fx and fx-open are only built via test-platforms step)
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

    // Create builtins static library at build time with minimal dependencies
    // Using a static library instead of object so we can bundle compiler_rt
    // (needed for 128-bit integer operations used by Dec type)
    const builtins_obj = b.addLibrary(.{
        .name = "roc_builtins",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
        .linkage = .static,
    });
    // Provide a no-op tracy stub so host_abi.zig can do @import("tracy") without
    // pulling in the real tracy module (which requires build_options).
    builtins_obj.root_module.addImport("tracy", b.addModule("tracy_stub", .{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
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
    // Add compiled builtins module for loading builtin types
    shim_lib.root_module.addImport("compiled_builtins", compiled_builtins_module);
    shim_lib.step.dependOn(&write_compiled_builtins.step);
    // Link against the pre-built builtins library
    shim_lib.linkLibrary(builtins_obj);
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

    // Copy builtins library for the host target for embedding into CLI
    // This is used by `roc build --backend=dev` to link the app object with builtins
    const copy_builtins = b.addUpdateSourceFiles();
    const host_builtins_filename = if (target.result.os.tag == .windows) "roc_builtins.lib" else "libroc_builtins.a";
    copy_builtins.addCopyFileToSource(builtins_obj.getEmittedBin(), b.pathJoin(&.{ "src/cli", host_builtins_filename }));
    exe.step.dependOn(&copy_builtins.step);

    // Add tracy support (required by parse/can/check modules)
    add_tracy(b, roc_modules.build_options, shim_lib, b.graph.host, false, flag_enable_tracy);

    // Cross-compile interpreter shim for all supported targets
    // This allows `roc build --target=X` to work for cross-compilation
    const cross_compile_shim_targets = [_]struct { name: []const u8, query: std.Target.Query }{
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

    for (cross_compile_shim_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        // Build builtins library for this target (with compiler_rt for 128-bit ops)
        const cross_builtins_obj = b.addLibrary(.{
            .name = b.fmt("roc_builtins_{s}", .{cross_target.name}),
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/builtins/static_lib.zig"),
                .target = cross_resolved_target,
                .optimize = optimize,
                .strip = strip,
                .omit_frame_pointer = omit_frame_pointer,
                .pic = true,
            }),
            .linkage = .static,
        });
        // Provide a no-op tracy stub (same as for host builtins above)
        cross_builtins_obj.root_module.addImport("tracy", b.addModule(
            b.fmt("tracy_stub_{s}", .{cross_target.name}),
            .{ .root_source_file = b.path("src/builtins/tracy_stub.zig") },
        ));
        cross_builtins_obj.bundle_compiler_rt = false;
        configureBackend(cross_builtins_obj, cross_resolved_target);

        // Build interpreter shim library for this target
        const cross_shim_lib = b.addLibrary(.{
            .name = b.fmt("roc_interpreter_shim_{s}", .{cross_target.name}),
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/interpreter_shim/main.zig"),
                .target = cross_resolved_target,
                .optimize = optimize,
                .strip = strip,
                .omit_frame_pointer = omit_frame_pointer,
                .pic = true,
            }),
            .linkage = .static,
        });
        configureBackend(cross_shim_lib, cross_resolved_target);

        // For wasm32, only add the modules needed by the interpreter shim
        // (compile, watch, lsp, repl, ipc use threading/file I/O not available on freestanding)
        if (cross_target.query.cpu_arch == .wasm32 and cross_target.query.os_tag == .freestanding) {
            cross_shim_lib.root_module.addImport("base", roc_modules.base);
            cross_shim_lib.root_module.addImport("collections", roc_modules.collections);
            cross_shim_lib.root_module.addImport("types", roc_modules.types);
            cross_shim_lib.root_module.addImport("builtins", roc_modules.builtins);
            cross_shim_lib.root_module.addImport("can", roc_modules.can);
            cross_shim_lib.root_module.addImport("check", roc_modules.check);
            cross_shim_lib.root_module.addImport("parse", roc_modules.parse);
            cross_shim_lib.root_module.addImport("layout", roc_modules.layout);
            cross_shim_lib.root_module.addImport("eval", roc_modules.eval);
            cross_shim_lib.root_module.addImport("reporting", roc_modules.reporting);
            cross_shim_lib.root_module.addImport("tracy", roc_modules.tracy);
            cross_shim_lib.root_module.addImport("build_options", roc_modules.build_options);
            cross_shim_lib.root_module.addImport("roc_target", roc_modules.roc_target);
            // Note: ipc module is NOT added for wasm32-freestanding as it uses POSIX calls
            // The interpreter shim main.zig has a stub for wasm32
        } else {
            roc_modules.addAll(cross_shim_lib);
        }
        cross_shim_lib.root_module.addImport("compiled_builtins", compiled_builtins_module);
        cross_shim_lib.step.dependOn(&write_compiled_builtins.step);
        cross_shim_lib.linkLibrary(cross_builtins_obj);
        cross_shim_lib.bundle_compiler_rt = true;

        // Copy to target-specific directory for embedding
        // Use .lib extension for Windows targets, .a for others
        const shim_ext = if (cross_target.query.os_tag == .windows) "roc_interpreter_shim.lib" else "libroc_interpreter_shim.a";
        const copy_cross_shim = b.addUpdateSourceFiles();
        copy_cross_shim.addCopyFileToSource(
            cross_shim_lib.getEmittedBin(),
            b.pathJoin(&.{ "src/cli/targets", cross_target.name, shim_ext }),
        );
        exe.step.dependOn(&copy_cross_shim.step);

        // Copy builtins library for this target for embedding into CLI
        // Used by `roc build --backend=dev --target=X` to link the app object with builtins
        const builtins_ext = if (cross_target.query.os_tag == .windows) "roc_builtins.lib" else "libroc_builtins.a";
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
    exe.addLibraryPath(.{ .cwd_relative = llvm_paths_exe.lib });
    exe.addIncludePath(.{ .cwd_relative = llvm_paths_exe.include });
    try addStaticLlvmOptionsToModule(exe.root_module);

    add_tracy(b, roc_modules.build_options, exe, target, true, tracy);

    exe.linkLibrary(zstd.artifact("zstd"));

    return exe;
}

fn install_and_run(
    b: *std.Build,
    no_bin: bool,
    exe: *Step.Compile,
    build_step: *Step,
    run_step: *Step,
    run_args: []const []const u8,
) void {
    if (run_step != build_step) {
        run_step.dependOn(build_step);
    }
    if (no_bin) {
        // No build, just build, don't actually install or run.
        build_step.dependOn(&exe.step);
        b.getInstallStep().dependOn(&exe.step);
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
    }
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
        const llvm_lib_dir = std.mem.trimRight(u8, b.run(&.{ llvm_config_path, "--libdir" }), "\n");
        const llvm_include_dir = std.mem.trimRight(u8, b.run(&.{ llvm_config_path, "--includedir" }), "\n");

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

/// Get the compiler version string for cache versioning.
/// Returns a string like "debug-abc12345" where abc12345 is the git commit SHA.
/// If git is not available, falls back to "debug-no-git" format.
fn getCompilerVersion(b: *std.Build, optimize: OptimizeMode) []const u8 {
    const build_mode = switch (optimize) {
        .Debug => "debug",
        .ReleaseSafe => "release-safe",
        .ReleaseFast => "release-fast",
        .ReleaseSmall => "release-small",
    };

    // Try to get git commit SHA using std.process.Child.run
    const result = std.process.Child.run(.{
        .allocator = b.allocator,
        .argv = &[_][]const u8{ "git", "rev-parse", "--short=8", "HEAD" },
    }) catch {
        // Git command failed, use fallback
        return std.fmt.allocPrint(b.allocator, "{s}-no-git", .{build_mode}) catch build_mode;
    };
    defer b.allocator.free(result.stdout);
    defer b.allocator.free(result.stderr);

    if (result.term == .Exited and result.term.Exited == 0) {
        // Git succeeded, use the commit SHA
        const commit_sha = std.mem.trim(u8, result.stdout, " \n\r\t");
        if (commit_sha.len > 0) {
            return std.fmt.allocPrint(b.allocator, "{s}-{s}", .{ build_mode, commit_sha }) catch
                std.fmt.allocPrint(b.allocator, "{s}-no-git", .{build_mode}) catch build_mode;
        }
    }

    // Git not available or failed, use fallback
    return std.fmt.allocPrint(b.allocator, "{s}-no-git", .{build_mode}) catch build_mode;
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

    const writer = assembly_buf.writer(b.allocator);
    const target_arch = target.result.cpu.arch;

    glibc_stub_build.generateComprehensiveStub(writer, target_arch) catch |err| {
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
