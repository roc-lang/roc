//! Host-side checks invoked as ordinary cached build Run steps.
const std = @import("std");
const builtin = @import("builtin");
const vendored_zig_marker = "Adapted from the Zig compiler";

const Context = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    environ_map: std.process.Environ.Map,
    args: []const []const u8,
};

fn fail(comptime format: []const u8, args: anytype) error{CheckFailed} {
    std.debug.print(format ++ "\n", args);
    return error.CheckFailed;
}

/// Dispatch the requested build check using its declared inputs and arguments.
pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.gpa);
    if (args.len < 2) return error.MissingCommand;
    const ctx = Context{ .allocator = init.arena.allocator(), .io = init.io, .environ_map = init.environ_map.*, .args = args[2..] };
    if (std.mem.eql(u8, args[1], "check-type-checker-patterns")) return CheckTypeCheckerPatternsStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-enum-from-int-zero")) return CheckEnumFromIntZeroStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-unused-suppression")) return CheckUnusedSuppressionStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-postcheck-architecture")) return CheckPostcheckArchitectureStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-wasm-builtin-routing")) return CheckWasmBuiltinRoutingStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-snapshot-diff")) return CheckSnapshotDiffStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-panic-usage")) return CheckPanicStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-cli-global-stdio")) return CheckCliGlobalStdioStep.run(ctx);
    if (std.mem.eql(u8, args[1], "coverage-summary")) return CoverageSummaryStep.run(ctx);
    if (std.mem.eql(u8, args[1], "check-test-asset-coverage-inner")) return CheckTestAssetCoverageStep.run(ctx);
    if (std.mem.eql(u8, args[1], "remove-dir-tree")) return RemoveDirTreeStep.run(ctx);
    if (std.mem.eql(u8, args[1], "fix-archive-padding")) return FixArchivePaddingStep.run(ctx);
    if (std.mem.eql(u8, args[1], "print-build-success")) return PrintBuildSuccessStep.run(ctx);
    if (std.mem.eql(u8, args[1], "coverage-unsupported")) {
        std.debug.print("Parser coverage is enabled only on Linux ARM64. Current platform: {s}\n", .{@tagName(builtin.os.tag)});
        return;
    }
    if (std.mem.eql(u8, args[1], "tests-summary")) return testSummary(ctx);
    if (std.mem.eql(u8, args[1], "check-builtin-bake-reproducible")) return checkBakes(ctx);
    return error.UnknownCommand;
}

const CheckTypeCheckerPatternsStep = struct {
    pub fn run(ctx: Context) !void {
        const allocator = ctx.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Recursively scan src/canonicalize/, src/check/, src/layout/, and src/eval/ for .zig files
        // TODO: uncomment "src/canonicalize" once its std.mem violations are fixed
        const dirs_to_scan = [_][]const u8{ "src/check", "src/layout", "src/eval" };
        for (dirs_to_scan) |dir_path| {
            const io = ctx.io;
            var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| {
                return fail("Failed to open {s} directory: {}", .{ dir_path, err });
            };
            defer dir.close(io);

            try scanDirectory(allocator, io, dir, dir_path, &violations);
        }

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("FORBIDDEN PATTERN DETECTED\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n\n", .{});

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

            std.debug.print("\n" ++ @as([80]u8, @splat('=')) ++ "\n", .{});

            return fail(
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
        // because ident indices are module-local—same nominal from different modules
        // has different Ident.Idx values, so we must compare the underlying strings
        .{ .file = "store.zig", .start = 340, .end = 355 },
        // Cross-module ident matching in cir_to_lir.zig requires string comparison
        // because platform and app modules have separate ident stores—the same alias
        // name has different Ident.Idx values across modules, so we must compare via text.
        .{ .file = "cir_to_lir.zig", .start = 110, .end = 115 },
        // inspected.zig resolves a type module's import statement from the caller's
        // module name, which arrives as text from outside this module's ident store.
        .{ .file = "inspected.zig", .start = 226, .end = 232 },
        // inspected.zig trims the trailing newline off a rendered report. This is
        // presentation text on its way out, not a type-checker comparison.
        .{ .file = "inspected.zig", .start = 2474, .end = 2474 },
        // inspected.zig converts a NUL-terminated dylib path from the linker into a
        // slice. Path bytes, not identifiers.
        .{ .file = "inspected.zig", .start = 3264, .end = 3275 },
        // inspected_run.zig dispatches on a hosted function's ABI symbol, which is
        // matched by name at the host boundary and has no Ident.Idx.
        .{ .file = "inspected_run.zig", .start = 109, .end = 109 },
        // Consumer compatibility excludes observation sinks by Zig field name at
        // compile time. These are compiler API fields, never Roc identifiers.
        .{ .file = "compile_time_finalization.zig", .start = 171, .end = 183 },
        // report.zig compares already-formatted diagnostic text only to avoid
        // printing two visually identical types. This is presentation logic,
        // not a type-checking or identifier comparison.
        .{ .file = "report.zig", .start = 615, .end = 615 },
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
                        // - std.mem.asBytes / bytesAsValue: type punning, not string comparison
                        // - std.mem.readInt / writeInt: fixed-width binary serialization
                        // - std.mem.reverse: reversing arrays, not string comparison
                        // - std.mem.alignForward: memory alignment arithmetic, not string comparison
                        // - std.mem.order: sort ordering (used by sort comparators), not string comparison
                        // - std.mem.copyForwards: byte copying, not string comparison
                        const is_allowed =
                            std.mem.startsWith(u8, after_match, "Allocator") or
                            std.mem.startsWith(u8, after_match, "Alignment") or
                            std.mem.startsWith(u8, after_match, "sort") or
                            std.mem.startsWith(u8, after_match, "asBytes") or
                            std.mem.startsWith(u8, after_match, "bytesAsValue(") or
                            std.mem.startsWith(u8, after_match, "readInt(") or
                            std.mem.startsWith(u8, after_match, "writeInt(") or
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

const CheckEnumFromIntZeroStep = struct {
    pub fn run(ctx: Context) !void {
        const allocator = ctx.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Recursively scan src/ for .zig files
        const io = ctx.io;
        var dir = std.Io.Dir.cwd().openDir(io, "src", .{ .iterate = true }) catch |err| {
            return fail("Failed to open src directory: {}", .{err});
        };
        defer dir.close(io);

        try scanDirectoryForEnumFromIntZero(allocator, io, dir, "src", &violations);

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("FORBIDDEN PATTERN: @fromBackingInt(0) or @enumFromInt(0)\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n\n", .{});

            std.debug.print(
                \\Using @fromBackingInt(0) or its legacy spelling @enumFromInt(0) is forbidden.
                \\
                \\WHY THIS RULE EXISTS:
                \\  Converting zero into an enum hides bugs and makes them harder to debug. It creates
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
                \\    .anno = @fromBackingInt(0), // placeholder - will be replaced
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

            std.debug.print("\n" ++ @as([80]u8, @splat('=')) ++ "\n", .{});

            return fail(
                "Found {d} zero integer to enum conversions. Using placeholder values like this has consistently led to bugs in this code base. " ++
                    "Do not use @fromBackingInt(0) or @enumFromInt(0), and do not uncritically replace it with another placeholder like .first. " ++
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

                    // Zig 0.17's formatter renames the legacy builtin spelling.
                    if (std.mem.find(u8, line, "@fromBackingInt(0)") != null or
                        std.mem.find(u8, line, "@fromBackingInt(@intCast(0))") != null or
                        std.mem.find(u8, line, "@enumFromInt(0)") != null or
                        std.mem.find(u8, line, "@enumFromInt(@intCast(0))") != null)
                    {
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

const CheckUnusedSuppressionStep = struct {
    pub fn run(ctx: Context) !void {
        const allocator = ctx.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Scan all src/ directories for .zig files
        const io = ctx.io;
        var dir = std.Io.Dir.cwd().openDir(io, "src", .{ .iterate = true }) catch |err| {
            return fail("Failed to open src/ directory: {}", .{err});
        };
        defer dir.close(io);

        try scanDirectoryForUnusedSuppression(allocator, io, dir, "src", &violations);

        if (violations.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("UNUSED VARIABLE SUPPRESSION DETECTED\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n\n", .{});

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

            std.debug.print("\n" ++ @as([80]u8, @splat('=')) ++ "\n", .{});

            return fail(
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

const CheckPostcheckArchitectureStep = struct {
    pub fn run(ctx: Context) !void {
        if (builtin.os.tag == .windows) {
            std.debug.print("Skipping post-check architecture check on Windows (perl not available)\n", .{});
            return;
        }

        var child_argv = std.ArrayList([]const u8).empty;
        defer child_argv.deinit(ctx.allocator);

        try child_argv.append(ctx.allocator, "perl");
        try child_argv.append(ctx.allocator, "ci/check_postcheck_architecture.pl");

        const io = ctx.io;
        var child = try std.process.spawn(io, .{
            .argv = child_argv.items,
            .environ_map = &ctx.environ_map,
        });
        const term = try child.wait(io);

        switch (term) {
            .exited => |code| {
                if (code != 0) {
                    return fail(
                        "Post-check architecture check failed. Run 'perl ci/check_postcheck_architecture.pl' to see details.",
                        .{},
                    );
                }
            },
            .signal, .stopped, .unknown => {
                return fail("ci/check_postcheck_architecture.pl terminated abnormally", .{});
            },
        }
    }
};

const CheckWasmBuiltinRoutingStep = struct {
    pub fn run(ctx: Context) !void {
        if (builtin.os.tag == .windows) {
            std.debug.print("Skipping WASM builtin routing check on Windows (perl not available)\n", .{});
            return;
        }

        const io = ctx.io;
        var child = try std.process.spawn(io, .{
            .argv = &.{ "perl", "ci/check_wasm_builtin_routing.pl" },
            .environ_map = &ctx.environ_map,
        });
        const term = try child.wait(io);
        switch (term) {
            .exited => |code| if (code != 0) {
                return fail(
                    "WASM builtin routing check failed. Run 'perl ci/check_wasm_builtin_routing.pl' to see details.",
                    .{},
                );
            },
            .signal, .stopped, .unknown => return fail("ci/check_wasm_builtin_routing.pl terminated abnormally", .{}),
        }
    }
};

const CheckSnapshotDiffStep = struct {
    const regen_hint = "Tracked snapshots changed after regeneration. Run 'zig build run-snapshot-tool' and commit the result.\n{s}";

    pub fn run(ctx: Context) !void {
        const io = ctx.io;

        // Prefer Git when the working directory is inside a Git work tree.
        const in_git = blk: {
            const probe = std.process.run(ctx.allocator, io, .{
                .argv = &.{ "git", "rev-parse", "--is-inside-work-tree" },
            }) catch break :blk false;
            defer ctx.allocator.free(probe.stdout);
            defer ctx.allocator.free(probe.stderr);
            break :blk probe.term == .exited and probe.term.exited == 0 and
                std.mem.eql(u8, std.mem.trim(u8, probe.stdout, " \n\r\t"), "true");
        };

        if (in_git) {
            const result = try std.process.run(ctx.allocator, io, .{
                .argv = &.{ "git", "diff", "--exit-code", "test/snapshots" },
            });
            defer ctx.allocator.free(result.stdout);
            defer ctx.allocator.free(result.stderr);
            if (!(result.term == .exited and result.term.exited == 0)) {
                return fail(regen_hint, .{result.stdout});
            }
            return;
        }

        // Fall back to JJ for a standalone JJ workspace (no Git backing).
        const in_jj = blk: {
            std.Io.Dir.cwd().access(io, ".jj", .{}) catch break :blk false;
            break :blk true;
        };

        if (in_jj) {
            const result = try std.process.run(ctx.allocator, io, .{
                .argv = &.{ "jj", "diff", "--summary", "test/snapshots" },
            });
            defer ctx.allocator.free(result.stdout);
            defer ctx.allocator.free(result.stderr);
            if (!(result.term == .exited and result.term.exited == 0)) {
                return fail("jj diff --summary test/snapshots failed:\n{s}", .{result.stderr});
            }
            if (std.mem.trim(u8, result.stdout, " \n\r\t").len != 0) {
                return fail(regen_hint, .{result.stdout});
            }
            return;
        }

        return fail("run-check-snapshots requires a Git or JJ workspace", .{});
    }
};

const CheckPanicStep = struct {

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

    pub fn run(ctx: Context) !void {
        const allocator = ctx.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        const io = ctx.io;

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
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("FORBIDDEN PATTERN: @panic / std.debug.panic in runtime code\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n\n", .{});

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

            std.debug.print("\n" ++ @as([80]u8, @splat('=')) ++ "\n", .{});

            return fail(
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

const CheckCliGlobalStdioStep = struct {
    pub fn run(ctx: Context) !void {
        const allocator = ctx.allocator;

        var violations = std.ArrayList(Violation).empty;
        defer violations.deinit(allocator);

        // Only scan src/cli/main.zig
        const file_path = "src/cli/main.zig";
        const io = ctx.io;
        const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(10 * 1024 * 1024)) catch |err| {
            return fail("Failed to read {s}: {}", .{ file_path, err });
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
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("GLOBAL STDIO USAGE DETECTED IN CLI\n", .{});
            std.debug.print(@as([80]u8, @splat('=')) ++ "\n\n", .{});

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

            std.debug.print("\n" ++ @as([80]u8, @splat('=')) ++ "\n", .{});

            return fail(
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

const CoverageSummaryStep = struct {
    coverage_dir: []const u8,
    exe_name: []const u8,
    label: []const u8,
    min_coverage: f64,

    /// Coverage is supported on:
    /// - macOS (ARM64 and x86_64): Uses libdwarf for DWARF parsing
    /// - Linux ARM64: Uses libdw (elfutils) for DWARF parsing
    ///
    /// Coverage is not enabled on Linux x86_64. With Zig 0.15.2 the x86_64 backend
    /// emitted DWARF .debug_line sections that libdw rejects ("invalid .debug_line
    /// section") for user compilation units while stdlib CUs parse, so kcov found
    /// only stdlib files. That has not been re-measured with kcov on Zig 0.16.
    /// See: https://github.com/roc-lang/roc/pull/8864 for investigation details.
    pub fn run(ctx: Context) !void {
        const allocator = ctx.allocator;
        const self = CoverageSummaryStep{ .coverage_dir = ctx.args[0], .exe_name = ctx.args[1], .label = ctx.args[2], .min_coverage = try std.fmt.parseFloat(f64, ctx.args[3]) };

        // Read kcov JSON output
        // kcov creates a subdirectory named after the executable (e.g., parse_unit_coverage/)
        // which contains the coverage.json file
        const json_path = try std.fmt.allocPrint(allocator, "{s}/{s}/coverage.json", .{ self.coverage_dir, self.exe_name });
        defer allocator.free(json_path);

        const io = ctx.io;
        const json_content = std.Io.Dir.cwd().readFileAlloc(io, json_path, allocator, .limited(10 * 1024 * 1024)) catch |err| {
            std.debug.print("\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("COVERAGE ERROR\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n\n", .{});
            std.debug.print("Could not open coverage JSON at {s}: {}\n", .{ json_path, err });
            std.debug.print("\nMake sure kcov is installed:\n", .{});
            std.debug.print("  - Linux: apt install kcov\n", .{});
            std.debug.print("  - macOS: brew install kcov\n\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});
            return;
        };
        defer allocator.free(json_content);

        // Parse and summarize coverage
        const result = try parseCoverageJson(allocator, json_content, self.label, self.coverage_dir);

        // Fail if kcov didn't capture any data - this indicates a problem with kcov
        if (result.total_lines == 0) {
            std.debug.print("\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("COVERAGE ERROR: NO DATA CAPTURED\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n\n", .{});
            std.debug.print("kcov reported 0 total lines - coverage data was not captured.\n", .{});
            std.debug.print("This indicates a problem with kcov or the binary format.\n\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});
            return fail("kcov failed to capture coverage data (0 total lines)", .{});
        }

        // Enforce minimum coverage threshold
        if (result.percent < self.min_coverage) {
            std.debug.print("\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});
            std.debug.print("COVERAGE CHECK FAILED\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n\n", .{});
            std.debug.print("{s} coverage is {d:.2}%, minimum required is {d:.2}%\n", .{ self.label, result.percent, self.min_coverage });
            std.debug.print("Add more tests to improve coverage before merging.\n\n", .{});
            std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});
            return fail("{s} coverage {d:.2}% is below minimum {d:.2}%", .{ self.label, result.percent, self.min_coverage });
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
        std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});
        std.debug.print("{s} CODE COVERAGE SUMMARY\n", .{label});
        std.debug.print(@as([60]u8, @splat('=')) ++ "\n\n", .{});

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

        std.debug.print("\n" ++ @as([60]u8, @splat('=')) ++ "\n", .{});
        std.debug.print("Full HTML report: {s}/index.html\n", .{coverage_dir});
        std.debug.print(@as([60]u8, @splat('=')) ++ "\n", .{});

        return .{ .percent = percent, .total_lines = total_lines };
    }

    const UncoveredFile = struct {
        file: []const u8,
        uncovered_lines: u64,
        total_lines: u64,
        percent: f64,
    };
};

const CheckTestAssetCoverageStep = struct {
    pub fn run(ctx: Context) !void {
        try checkTestAssetCoverage(ctx);
    }
};

const RemoveDirTreeStep = struct {
    dir_path: []const u8,

    pub fn run(ctx: Context) !void {
        const self = RemoveDirTreeStep{ .dir_path = ctx.args[0] };
        const io = ctx.io;
        std.Io.Dir.cwd().deleteTree(io, self.dir_path) catch {};
    }
};

const FixArchivePaddingStep = struct {
    pub fn run(ctx: Context) !void {
        if (ctx.args.len != 2) return error.ExpectedInputAndOutput;
        const input = try std.Io.Dir.cwd().readFileAlloc(ctx.io, ctx.args[0], ctx.allocator, .unlimited);
        var bytes = std.ArrayList(u8).fromOwnedSlice(input);
        if (!std.mem.startsWith(u8, bytes.items, "!<arch>\n")) return error.InvalidArchiveMagic;

        // Retain the #30572 correction for a missing final member padding byte.
        // Validate the complete archive; missing or malformed declared inputs
        // must fail instead of creating a successful but unusable cached output.
        var offset: usize = 8;
        while (offset < bytes.items.len) {
            if (bytes.items.len - offset < 60) return error.TruncatedArchiveHeader;
            const header = bytes.items[offset..][0..60];
            if (!std.mem.eql(u8, header[58..60], "`\n")) return error.InvalidArchiveHeader;
            const size = try std.fmt.parseInt(usize, std.mem.trim(u8, header[48..58], " "), 10);
            const end = try std.math.add(usize, try std.math.add(usize, offset, 60), size);
            if (end > bytes.items.len) return error.TruncatedArchiveMember;
            if (size % 2 == 1 and end == bytes.items.len) try bytes.append(ctx.allocator, '\n');
            offset = try std.math.add(usize, end, size % 2);
        }
        if (offset != bytes.items.len) return error.InvalidArchivePadding;
        try std.Io.Dir.cwd().writeFile(ctx.io, .{ .sub_path = ctx.args[1], .data = bytes.items });
    }
};

const PrintBuildSuccessStep = struct {
    pub fn run(_: Context) !void {
        std.debug.print("Build succeeded!\n", .{});
    }
};

const TestAssetCoverageDir = struct {
    dir: []const u8,
    spec_files: []const []const u8,
};

// Every app .roc file in these directories (including nested subdirectories) must
// be named by at least one of its spec sources; otherwise it is dead test data no
// runner executes.
// Module and platform .roc files are dependencies of apps, so only files whose
// first non-comment line is an `app` header are required to be covered.
const test_asset_coverage_dirs = [_]TestAssetCoverageDir{
    .{ .dir = "test/fx", .spec_files = &.{
        "src/cli/test/fx_platform_test.zig",
        "src/cli/test/fx_test_specs.zig",
        "src/cli/test/parallel_cli_runner.zig",
    } },
    .{ .dir = "test/fx-open", .spec_files = &.{
        "src/cli/test/platform_config.zig",
        "src/cli/test/parallel_cli_runner.zig",
    } },
    .{ .dir = "test/cli", .spec_files = &.{
        "src/cli/test/parallel_cli_runner.zig",
        "src/compile/test/embedding_smoke.zig",
    } },
    .{ .dir = "test/package-effect-boundary", .spec_files = &.{
        "src/cli/test/parallel_cli_runner.zig",
    } },
    .{ .dir = "test/str", .spec_files = &.{
        "src/cli/test/platform_config.zig",
        "src/cli/test/parallel_cli_runner.zig",
        "src/compile/coordinator.zig",
    } },
    .{ .dir = "test/echo", .spec_files = &.{
        "src/cli/test/parallel_cli_runner.zig",
        "src/eval/test/builtin_doc_tests.zig",
    } },
};

fn isRocAppFile(contents: []const u8) bool {
    var line_iter = std.mem.splitScalar(u8, contents, '\n');
    while (line_iter.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (line.len == 0) continue;
        if (line[0] == '#') continue;
        return std.mem.startsWith(u8, line, "app");
    }
    return false;
}

fn checkTestAssetCoverage(ctx: Context) !void {
    std.debug.print("---- checking test asset coverage ----\n", .{});

    const allocator = ctx.allocator;
    const io = ctx.io;

    var total_missing: usize = 0;
    var total_checked: usize = 0;

    for (test_asset_coverage_dirs) |cfg| {
        var asset_dir = try std.Io.Dir.cwd().openDir(io, cfg.dir, .{ .iterate = true });
        defer asset_dir.close(io);

        var app_files = std.ArrayList([]const u8).empty;
        defer {
            for (app_files.items) |file| {
                allocator.free(file);
            }
            app_files.deinit(allocator);
        }

        // Fixtures group each case in its own subdirectory, so walk the whole
        // tree instead of only the top level. Paths stay relative to cfg.dir and
        // are always '/'-separated so they compare directly against the paths
        // written in the spec sources.
        var walker = try asset_dir.walk(allocator);
        defer walker.deinit();
        while (try walker.next(io)) |entry| {
            if (entry.kind != .file or !std.mem.endsWith(u8, entry.basename, ".roc")) continue;
            const contents = try entry.dir.readFileAlloc(io, entry.basename, allocator, .limited(1024 * 1024));
            defer allocator.free(contents);
            if (!isRocAppFile(contents)) continue;
            const rel_path = try allocator.dupe(u8, entry.path);
            std.mem.replaceScalar(u8, rel_path, std.fs.path.sep, '/');
            try app_files.append(allocator, rel_path);
        }

        std.mem.sort([]const u8, app_files.items, {}, struct {
            fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                return std.mem.order(u8, lhs, rhs) == .lt;
            }
        }.lessThan);

        var tested_files = std.StringHashMap(void).init(allocator);
        defer {
            var key_iter = tested_files.keyIterator();
            while (key_iter.next()) |key| {
                allocator.free(key.*);
            }
            tested_files.deinit();
        }

        const dir_prefix = try std.fmt.allocPrint(allocator, "{s}/", .{cfg.dir});
        defer allocator.free(dir_prefix);

        for (cfg.spec_files) |spec_file_path| {
            const spec_contents = std.Io.Dir.cwd().readFileAlloc(io, spec_file_path, allocator, .limited(4 * 1024 * 1024)) catch |err| {
                return fail("could not read spec source {s}: {}", .{ spec_file_path, err });
            };
            defer allocator.free(spec_contents);

            var line_iter = std.mem.splitScalar(u8, spec_contents, '\n');
            while (line_iter.next()) |full_line| {
                // A mention inside a `//` comment is not a live spec entry.
                const line = if (std.mem.find(u8, full_line, "//")) |comment_idx|
                    full_line[0..comment_idx]
                else
                    full_line;

                var search_start: usize = 0;
                while (std.mem.findPos(u8, line, search_start, dir_prefix)) |idx| {
                    const rest_of_line = line[idx..];
                    if (std.mem.find(u8, rest_of_line, ".roc")) |roc_pos| {
                        const full_path = rest_of_line[0 .. roc_pos + 4];
                        // Path relative to cfg.dir; may name a file in a subdirectory.
                        const filename = full_path[dir_prefix.len..];
                        const duped_filename = try allocator.dupe(u8, filename);
                        if (tested_files.contains(duped_filename)) {
                            allocator.free(duped_filename);
                        } else {
                            try tested_files.put(duped_filename, {});
                        }
                    }
                    search_start = idx + 1;
                }
            }
        }

        var missing_count: usize = 0;
        for (app_files.items) |app_file| {
            total_checked += 1;
            if (!tested_files.contains(app_file)) {
                if (missing_count == 0) {
                    std.debug.print("\nERROR: app .roc files in {s}/ with no spec entry:\n", .{cfg.dir});
                }
                std.debug.print("  - {s}/{s}\n", .{ cfg.dir, app_file });
                missing_count += 1;
            }
        }
        if (missing_count > 0) {
            std.debug.print("Add a spec entry in one of:\n", .{});
            for (cfg.spec_files) |spec_file_path| {
                std.debug.print("  {s}\n", .{spec_file_path});
            }
            std.debug.print("or delete the unused file(s).\n", .{});
        }
        total_missing += missing_count;
    }

    if (total_missing > 0) {
        return fail("{d} app .roc file(s) have no spec entry", .{total_missing});
    }

    std.debug.print("All {d} app .roc files across {d} asset directories are covered.\n", .{ total_checked, test_asset_coverage_dirs.len });
}

fn checkBakes(ctx: Context) !void {
    const names = [_][]const u8{ "Builtin.bin", "builtin_indices.zig", "Builtin.artifact.bin" };
    var baseline: [names.len][]const u8 = undefined;
    for (0..3) |bake| {
        for (names, 0..) |name, index| {
            const bytes = try std.Io.Dir.cwd().readFileAlloc(ctx.io, ctx.args[bake * names.len + index], ctx.allocator, .limited(256 * 1024 * 1024));
            if (bake == 0) {
                baseline[index] = bytes;
            } else {
                const expected = baseline[index];
                if (expected.len != bytes.len) return fail("{s} size differs in bake {d}: {d} vs {d}", .{ name, bake, expected.len, bytes.len });
                for (expected, bytes, 0..) |a, b, offset| {
                    if (a != b) return fail("{s} bake {d} differs at byte {d}", .{ name, bake, offset });
                }
                ctx.allocator.free(bytes);
            }
        }
    }
    for (baseline) |bytes| ctx.allocator.free(bytes);
}

fn testSummary(ctx: Context) !void {
    _ = try std.fmt.parseInt(u64, ctx.args[0], 10);
    const filter_count = try std.fmt.parseInt(usize, ctx.args[1], 10);
    var filter_list: std.ArrayList([]const u8) = .empty;
    try filter_list.appendSlice(ctx.allocator, ctx.args[2..][0..filter_count]);
    const remaining = ctx.args[2 + filter_count ..];
    var boundary: usize = remaining.len;
    for (remaining, 0..) |arg, index| {
        if (std.mem.eql(u8, arg, "--")) {
            boundary = index;
            break;
        }
    }
    var index_arg = @min(boundary + 1, remaining.len);
    while (index_arg < remaining.len) : (index_arg += 1) {
        const arg = remaining[index_arg];
        if (std.mem.eql(u8, arg, "--test-filter")) {
            index_arg += 1;
            if (index_arg == remaining.len) return error.MissingFilter;
            try filter_list.append(ctx.allocator, remaining[index_arg]);
        } else if (std.mem.startsWith(u8, arg, "--test-filter=")) {
            try filter_list.append(ctx.allocator, arg["--test-filter=".len..]);
        }
    }
    const filters = filter_list.items;
    const reports = remaining[0..boundary];
    var passed: u64 = 0;
    var previous_module: []const u8 = "";
    var previous_name: []const u8 = "";
    const spaces: [256]u8 = @splat(' ');
    var index: usize = 0;
    while (index < reports.len) : (index += 2) {
        const module = reports[index];
        const contents = try std.Io.Dir.cwd().readFileAlloc(ctx.io, reports[index + 1], ctx.allocator, .limited(16 * 1024 * 1024));
        // Keep report bytes alive while the next row compares its prefix.
        var rows = std.mem.splitScalar(u8, contents, '\n');
        while (rows.next()) |row| {
            if (row.len < 2) continue;
            const name = row[2..];
            if (filters.len == 0) {
                if (row[0] == '1') passed += 1;
                continue;
            }
            var matched = false;
            for (filters) |filter| {
                if (std.mem.find(u8, name, filter) != null) matched = true;
            }
            if (!matched) continue;
            if (row[0] == '1') passed += 1;
            var dot: usize = 0;
            if (std.mem.eql(u8, module, previous_module)) {
                for (0..@min(name.len, previous_name.len)) |i| {
                    if (name[i] != previous_name[i]) break;
                    if (name[i] == '.') dot = i;
                }
            }
            if (dot > 0) {
                const indent = @min(2 + module.len + 2 + dot, spaces.len);
                std.debug.print("{s}{s}\n", .{ spaces[0..indent], name[dot..] });
            } else {
                std.debug.print("  {s}: {s}\n", .{ module, name });
            }
            previous_module = module;
            previous_name = name;
        }
    }
    if (passed == 0) {
        std.debug.print("No tests ran (all tests filtered out).\n", .{});
    } else {
        std.debug.print("All {d} tests passed.\n", .{passed});
    }
}
