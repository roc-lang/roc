//! Tests every ```roc code block in src/build/roc/Builtin.roc.
//!
//! For each block:
//!   * It must pass `roc check` (driven by `parseAndCheckProgramForProblems`).
//!   * If the block contains only top-level `expect` statements, those expects
//!     are executed through checked-artifact compile-time evaluation.
//!   * Otherwise, the block is evaluated through `compileInspectedProgram`.
//!
//! When a block fails, the source is written to a debug file under
//! `test/echo/` and the `roc` binary (at `./zig-out/bin/roc`) is invoked to
//! confirm the same failure reproduces, so the test never reports phantom
//! in-memory errors. If the binary disagrees, the test escalates the issue.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const testing = std.testing;

/// Forking is needed so a single crashing block (e.g. annotation-only function
/// reference that hits a compiler invariant) doesn't kill the whole test.
const has_fork = switch (builtin.os.tag) {
    .windows => false,
    else => true,
};

const eval_mod = @import("eval");
const test_helpers = eval_mod.test_helpers;
const collections = @import("collections");
const CoreCtx = @import("ctx").CoreCtx;

const Allocator = std.mem.Allocator;

const BuiltinDocTestError = test_helpers.TestHelperError || eval_mod.BuiltinModules.InitError || std.process.SpawnError || std.process.Child.WaitError || std.Io.File.OpenError || std.Io.File.Reader.Error || std.Io.File.Writer.Error || std.Io.Dir.WriteFileError || std.Io.Dir.CreateDirPathError || CoreCtx.WriteError || error{
    PhantomFailuresDetected,
    DocBlockFailures,
    SkipZigTest,
    SigactionFailed,
    TestExpectedEqual,
    TestUnexpectedResult,
};

/// Builtin module viewed and published once in the parent test process and
/// reused (read-only) by every block: directly for the in-parent check phase,
/// and — via fork copy-on-write — by the child eval phase. Reusing it avoids
/// re-creating the view and re-publishing the Builtin module ~700 times. File-scope
/// because the C-ABI child work fns dispatched through `runInChild` cannot
/// capture it, and because it must already be in the parent's address space
/// when `fork()` copies it into each child.
var shared_builtins: ?*eval_mod.BuiltinModules = null;

/// Borrow the shared Builtin as a `PrePublishedBuiltin`, or null before setup.
fn prePublishedBuiltin() ?test_helpers.PrePublishedBuiltin {
    const bm = shared_builtins orelse return null;
    return .{
        .env = bm.builtin_module.env,
        .indices = bm.builtin_indices,
        .artifact = &bm.checked_artifact,
    };
}

/// Path to the Builtin.roc file, relative to the project root (the directory
/// where `zig build run-test-zig` is invoked from).
const builtin_roc_path = "src/build/roc/Builtin.roc";
/// Where to write debug files when a block fails.
const debug_dir = "test/echo";
/// Path to the compiled roc binary used for reproducing failures.
const roc_binary = "./zig-out/bin/roc";

test "numeric try API has no legacy suffix" {
    const allocator = base.defaultGpa();
    const ctx = CoreCtx.default(allocator, allocator, std.testing.io);
    const builtin_source = try ctx.readFile(builtin_roc_path, allocator);
    defer allocator.free(builtin_source);

    const legacy_suffix = "_" ++ "checked";
    try testing.expectEqual(null, std.mem.find(u8, builtin_source, legacy_suffix));
}

const BlockKind = enum {
    /// All top-level statements are `expect` — runs via `roc test` logic.
    expects_only,
    /// At least one top-level definition (`name = ...`) — wrap as a module
    /// with an auto-generated `main` and evaluate.
    module_with_def,
    /// Top-level expressions only (no defs, no expects) — each expression is
    /// evaluated independently.
    expression_block,
};

const Block = struct {
    /// 1-indexed line in Builtin.roc where the opening ```roc fence sits.
    start_line: usize,
    /// Stripped Roc source (owned).
    source: []u8,
    /// Name of the last top-level `name =` definition, if any. Points into
    /// `source` and is valid for the lifetime of the block.
    last_def_name: ?[]const u8,
    kind: BlockKind,

    fn deinit(self: *Block, allocator: Allocator) void {
        allocator.free(self.source);
    }
};

fn extractBlocks(allocator: Allocator, source: []const u8) BuiltinDocTestError![]Block {
    var blocks = std.ArrayList(Block).empty;
    errdefer {
        for (blocks.items) |*b| b.deinit(allocator);
        blocks.deinit(allocator);
    }

    var block_lines = std.ArrayList([]const u8).empty;
    defer block_lines.deinit(allocator);

    var in_block: bool = false;
    var block_start_line: usize = 0;
    var line_no: usize = 0;

    var iter = std.mem.splitScalar(u8, source, '\n');
    while (iter.next()) |line| {
        line_no += 1;
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (!in_block) {
            if (std.mem.eql(u8, trimmed, "## ```roc")) {
                in_block = true;
                block_start_line = line_no;
                block_lines.clearRetainingCapacity();
            }
            continue;
        }
        if (std.mem.eql(u8, trimmed, "## ```")) {
            in_block = false;
            const stripped = try assembleStripped(allocator, block_lines.items);
            errdefer allocator.free(stripped);

            const analysis = analyze(stripped);
            try blocks.append(allocator, .{
                .start_line = block_start_line,
                .source = stripped,
                .last_def_name = analysis.last_def_name,
                .kind = classify(analysis),
            });
            continue;
        }
        try block_lines.append(allocator, line);
    }

    return blocks.toOwnedSlice(allocator);
}

fn assembleStripped(allocator: Allocator, lines: []const []const u8) BuiltinDocTestError![]u8 {
    var stripped_lines = std.ArrayList([]const u8).empty;
    defer stripped_lines.deinit(allocator);
    for (lines) |line| try stripped_lines.append(allocator, stripDocPrefix(line));

    // Dedent: find the minimum leading whitespace across non-empty lines and
    // remove that prefix uniformly. This handles cases where the doc comment
    // body was written with `##  body` (two spaces after `##`), which leaves
    // a stray leading space the parser would treat as continuation.
    var min_indent: usize = std.math.maxInt(usize);
    for (stripped_lines.items) |l| {
        if (l.len == 0) continue;
        var i: usize = 0;
        while (i < l.len and (l[i] == ' ' or l[i] == '\t')) : (i += 1) {}
        if (i == l.len) continue; // whitespace-only line
        if (i < min_indent) min_indent = i;
        if (min_indent == 0) break;
    }
    if (min_indent == std.math.maxInt(usize)) min_indent = 0;

    var buf = std.ArrayList(u8).empty;
    errdefer buf.deinit(allocator);
    for (stripped_lines.items, 0..) |line, i| {
        const dedented = if (line.len > min_indent) line[min_indent..] else "";
        try buf.appendSlice(allocator, dedented);
        if (i + 1 != stripped_lines.items.len) try buf.append(allocator, '\n');
    }
    return buf.toOwnedSlice(allocator);
}

/// Drop leading indentation, the literal `##`, and at most one following space.
/// Also trims trailing whitespace.
fn stripDocPrefix(line: []const u8) []const u8 {
    var i: usize = 0;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {}
    if (i + 1 >= line.len or line[i] != '#' or line[i + 1] != '#') {
        return std.mem.trimEnd(u8, line[i..], " \t\r");
    }
    i += 2;
    if (i < line.len and line[i] == ' ') i += 1;
    return std.mem.trimEnd(u8, line[i..], " \t\r");
}

const Analysis = struct {
    has_expect: bool = false,
    has_def: bool = false,
    has_other: bool = false,
    statement_count: usize = 0,
    last_def_name: ?[]const u8 = null,
};

fn analyze(source: []const u8) Analysis {
    var result: Analysis = .{};
    var iter = std.mem.splitScalar(u8, source, '\n');
    while (iter.next()) |line| {
        if (line.len == 0) continue;
        // Continuation (indented) or comment lines are part of a previous
        // top-level statement.
        if (line[0] == ' ' or line[0] == '\t') continue;
        if (line[0] == '#') continue;

        result.statement_count += 1;
        if (startsWithKeyword(line, "expect")) {
            result.has_expect = true;
        } else if (extractDefName(line)) |name| {
            result.has_def = true;
            result.last_def_name = name;
        } else {
            result.has_other = true;
        }
    }
    return result;
}

fn classify(a: Analysis) BlockKind {
    // Any block with expects is treated as a test — defs and helper code in
    // the same block are supporting context for those expects.
    if (a.has_expect) return .expects_only;
    if (a.has_def) return .module_with_def;
    return .expression_block;
}

/// Returns true if `source` contains any effectful function call, identified
/// by a `name!` suffix on an identifier (the Roc convention for effects).
/// Distinguishes from the prefix logical-not `!x` and the inequality operator
/// `!=` by requiring the `!` to follow an identifier character and to not be
/// immediately followed by `=`.
fn containsEffectfulCall(source: []const u8) bool {
    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        if (source[i] != '!') continue;
        if (i + 1 < source.len and source[i + 1] == '=') continue;
        if (i == 0) continue;
        const prev = source[i - 1];
        if (std.ascii.isAlphanumeric(prev) or prev == '_') return true;
    }
    return false;
}

fn startsWithKeyword(s: []const u8, word: []const u8) bool {
    if (!std.mem.startsWith(u8, s, word)) return false;
    if (s.len == word.len) return true;
    const next = s[word.len];
    return !std.ascii.isAlphanumeric(next) and next != '_';
}

fn extractDefName(line: []const u8) ?[]const u8 {
    var end: usize = 0;
    while (end < line.len) : (end += 1) {
        const c = line[end];
        if (!(std.ascii.isAlphanumeric(c) or c == '_')) break;
    }
    if (end == 0) return null;
    const name = line[0..end];
    var rest = line[end..];
    while (rest.len > 0 and (rest[0] == ' ' or rest[0] == '\t')) {
        rest = rest[1..];
    }
    if (rest.len == 0) return null;
    if (rest[0] == '=' or rest[0] == ':') return name;
    return null;
}

/// Splits the source into top-level statements (groups of lines starting at
/// column 0, plus any indented/comment continuation lines until the next
/// top-level start or the end of source). Tracks `{`/`[`/`(` nesting so a
/// closing brace on its own line is still treated as a continuation of the
/// statement that opened the brace. Each returned slice points into `source`.
fn splitTopLevelStatements(allocator: Allocator, source: []const u8) BuiltinDocTestError![][]const u8 {
    var statements = std.ArrayList([]const u8).empty;
    errdefer statements.deinit(allocator);

    var current_start: ?usize = null;
    var current_end: usize = 0;
    var depth: i32 = 0;

    var pos: usize = 0;
    var iter = std.mem.splitScalar(u8, source, '\n');
    while (iter.next()) |line| {
        const line_start = pos;
        pos += line.len + 1; // +1 for the newline (over-counts on last line; harmless)

        const is_empty = line.len == 0;
        const is_continuation = !is_empty and (line[0] == ' ' or line[0] == '\t');
        const is_comment = !is_empty and line[0] == '#';

        // Lines inside a brace/bracket/paren are part of the current statement
        // regardless of indentation.
        const inside_brace = depth > 0 and current_start != null;

        if (is_empty and !inside_brace) {
            if (current_start) |start| {
                try statements.append(allocator, source[start..current_end]);
                current_start = null;
            }
            continue;
        }
        if (is_empty) continue;

        if ((is_continuation or is_comment) or inside_brace) {
            if (current_start != null) {
                current_end = line_start + line.len;
                if (!is_comment) depth += braceDelta(line);
            }
            continue;
        }
        // New top-level statement at column 0.
        if (current_start) |start| {
            try statements.append(allocator, source[start..current_end]);
        }
        current_start = line_start;
        current_end = line_start + line.len;
        depth = braceDelta(line);
        if (depth < 0) depth = 0; // defensive
    }
    if (current_start) |start| {
        try statements.append(allocator, source[start..current_end]);
    }
    return statements.toOwnedSlice(allocator);
}

/// Net `{` minus `}` (and bracket / paren) count on a line. Naive — does not
/// understand string literals or comments — but good enough for the doc
/// snippets in Builtin.roc where every brace pair lives on its own.
fn braceDelta(line: []const u8) i32 {
    var d: i32 = 0;
    for (line) |c| switch (c) {
        '{', '[', '(' => d += 1,
        '}', ']', ')' => d -= 1,
        else => {},
    };
    return d;
}

const Failure = struct {
    block_index: usize,
    start_line: usize,
    stage: []const u8,
    message: []u8,
    crashed: bool,

    fn deinit(self: *Failure, allocator: Allocator) void {
        allocator.free(self.message);
    }
};

fn dupeErr(allocator: Allocator, comptime fmt: []const u8, args: anytype) BuiltinDocTestError![]u8 {
    return std.fmt.allocPrint(allocator, fmt, args);
}

const ForkOutcome = union(enum) {
    success,
    /// Child exited non-zero with an error message read from its pipe.
    failed: []u8,
    /// Child terminated by a signal — typically a panic / abort triggered by a
    /// compiler invariant when the block exercises unimplemented code.
    crashed: u8,
    /// fork() / pipe() failed; fall through to inline execution.
    fork_unavailable,
};

/// Function pointer signature for the work performed in the child process.
const ChildWorkFn = *const fn (allocator: Allocator, source: []const u8) BuiltinDocTestError!?[]u8;

/// Run `work` in a forked child process, isolating crashes from the parent.
/// `work` returns null on success or an owned error string on a clean failure.
/// On platforms without fork (Windows), runs inline; the caller must accept
/// the risk of a process-killing crash there.
fn runInChild(allocator: Allocator, work: ChildWorkFn, source: []const u8) ForkOutcome {
    if (comptime !has_fork) {
        const result = work(allocator, source) catch |err| {
            const owned = std.fmt.allocPrint(allocator, "child error: {s}", .{@errorName(err)}) catch {
                return .{ .failed = "" };
            };
            return .{ .failed = owned };
        };
        if (result) |msg| return .{ .failed = msg };
        return .success;
    }

    var pipe_fds: [2]std.c.fd_t = undefined;
    if (std.c.pipe(&pipe_fds) != 0) return .fork_unavailable;
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = std.c.fork();
    if (fork_result < 0) {
        _ = std.c.close(pipe_read);
        _ = std.c.close(pipe_write);
        return .fork_unavailable;
    }

    if (fork_result == 0) {
        // Child path.
        _ = std.c.close(pipe_read);

        // Silence the child's stdout AND stderr by pointing both at /dev/null.
        // stderr: a crashing child writes its panic stack there, which would
        // otherwise flood the parent test's output (the parent records the
        // signal number via waitpid). stdout: under `zig build`'s test runner
        // the test process owns fd 1 as the `--listen` IPC pipe; any byte the
        // child's in-process roc evaluation writes to stdout would corrupt that
        // protocol and fail the command even though every test passed. The child
        // returns its result over the explicit `pipe_write`, never via stdout.
        const dev_null = std.c.open("/dev/null", .{ .ACCMODE = .WRONLY });
        if (dev_null >= 0) {
            _ = std.c.dup2(dev_null, 1);
            _ = std.c.dup2(dev_null, 2);
            _ = std.c.close(dev_null);
        }

        var arena = collections.SingleThreadArena.init(base.defaultGpa());
        const child_alloc = arena.allocator();

        const result = work(child_alloc, source) catch |err| {
            const name = @errorName(err);
            _ = std.c.write(pipe_write, name.ptr, name.len);
            _ = std.c.close(pipe_write);
            std.c._exit(1);
        };
        if (result) |msg| {
            _ = std.c.write(pipe_write, msg.ptr, msg.len);
            _ = std.c.close(pipe_write);
            std.c._exit(1);
        }
        _ = std.c.close(pipe_write);
        std.c._exit(0);
    }

    // Parent path.
    _ = std.c.close(pipe_write);

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);
    while (true) {
        var read_buf: [4096]u8 = undefined;
        const bytes = std.c.read(pipe_read, &read_buf, read_buf.len);
        if (bytes == 0) break; // EOF
        if (bytes < 0) {
            // std.c.read does not retry on EINTR like std.posix.read did; do
            // it ourselves so a signal during readout doesn't truncate the
            // child's error message.
            if (@as(std.c.E, @enumFromInt(std.c._errno().*)) == .INTR) continue;
            break;
        }
        buf.appendSlice(allocator, read_buf[0..@intCast(bytes)]) catch break;
    }
    _ = std.c.close(pipe_read);

    var status: c_int = 0;
    // std.c.waitpid does not retry on EINTR like std.posix.waitpid did. If we
    // don't loop here, an interrupted wait returns -1 with `status` left at 0,
    // which would then be misread below as "child exited cleanly with code 0"
    // and a real crash would be silently reported as success.
    while (true) {
        const rc = std.c.waitpid(fork_result, &status, 0);
        if (rc >= 0) break;
        if (@as(std.c.E, @enumFromInt(std.c._errno().*)) == .INTR) continue;
        return .fork_unavailable;
    }
    const sig: u8 = @truncate(@as(u32, @bitCast(status)) & 0x7f);
    if (sig != 0) {
        return .{ .crashed = sig };
    }
    const exit_code: u8 = @truncate((@as(u32, @bitCast(status)) >> 8) & 0xff);
    if (exit_code != 0) {
        const owned = buf.toOwnedSlice(allocator) catch return .{ .failed = "" };
        return .{ .failed = owned };
    }
    return .success;
}

fn checkPassed(resources: *test_helpers.ProblemResources) bool {
    if (resources.main.checker.problems.problems.items.len != 0) return false;
    if (resources.main.module_env.types.containsErrContent()) return false;
    // Canonicalization diagnostics.
    const diag_indices = resources.main.module_env.store.sliceDiagnostics(
        resources.main.module_env.diagnostics,
    );
    if (diag_indices.len != 0) return false;
    return true;
}

/// Run the check stage for `source` and return null on success, or an owned
/// error message on failure.
fn runCheck(
    allocator: Allocator,
    source_kind: test_helpers.SourceKind,
    source: []const u8,
) BuiltinDocTestError!?[]u8 {
    var resources = (if (prePublishedBuiltin()) |ppb|
        test_helpers.parseAndCheckProgramForProblemsWithBuiltin(allocator, source_kind, source, &.{}, ppb)
    else
        test_helpers.parseAndCheckProgramForProblems(allocator, source_kind, source, &.{})) catch |err|
        return try dupeErr(allocator, "parseAndCheckProgramForProblems: {s}", .{@errorName(err)});
    defer resources.deinit(allocator);

    if (!checkPassed(&resources)) {
        return try dupeErr(allocator, "check produced diagnostics or problems", .{});
    }
    return null;
}

/// Run an expect-only block through checked-artifact compile-time evaluation.
/// This matches the path used by `roc test` for top-level expects.
fn runExpects(allocator: Allocator, source: []const u8) BuiltinDocTestError!?[]u8 {
    const outcome = (if (prePublishedBuiltin()) |ppb|
        test_helpers.publishProgramForComptimeProblemsWithBuiltin(allocator, .module, source, &.{}, ppb)
    else
        test_helpers.publishProgramForComptimeProblems(allocator, .module, source, &.{})) catch |err| {
        return try dupeErr(allocator, "publishProgramForComptimeProblems: {s}", .{@errorName(err)});
    };

    return switch (outcome) {
        .no_problems => null,
        .comptime_problems => try dupeErr(allocator, "at least one expect failed", .{}),
    };
}

fn runEval(
    allocator: Allocator,
    source_kind: test_helpers.SourceKind,
    source: []const u8,
) BuiltinDocTestError!?[]u8 {
    var compiled = compileNative(allocator, source_kind, source) catch |err|
        return try dupeErr(allocator, "compileInspectedProgram: {s}", .{@errorName(err)});
    defer compiled.deinit(allocator);

    const inspected = test_helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered) catch |err|
        return try dupeErr(allocator, "lirInterpreterInspectedStr: {s}", .{@errorName(err)});
    allocator.free(inspected);
    return null;
}

/// Compile a block for the native target only, reusing the shared Builtin when
/// it is available. The harness only evaluates `compiled.lowered` through the
/// native LIR interpreter, so lowering wasm (as the generic
/// `compileInspectedProgram` does) is pure waste here.
fn compileNative(
    allocator: Allocator,
    source_kind: test_helpers.SourceKind,
    source: []const u8,
) BuiltinDocTestError!test_helpers.CompiledTargetProgram {
    if (prePublishedBuiltin()) |ppb| {
        return test_helpers.compileInspectedProgramForTargetWithBuiltin(
            allocator,
            std.testing.io,
            source_kind,
            source,
            &.{},
            .native,
            ppb,
        );
    }
    return test_helpers.compileInspectedProgramForTarget(
        allocator,
        std.testing.io,
        source_kind,
        source,
        &.{},
        .native,
    );
}

/// Result of processing a block.
const ProcessResult = union(enum) {
    success,
    failed: []u8, // owned message
    crashed: u8, // signal number
};

/// Worker bodies invoked inside the forked child for each block kind.
fn workerExpects(allocator: Allocator, source: []const u8) BuiltinDocTestError!?[]u8 {
    return runExpects(allocator, source);
}
fn workerEvalModule(allocator: Allocator, source: []const u8) BuiltinDocTestError!?[]u8 {
    return runEval(allocator, .module, source);
}
fn workerEvalExpr(allocator: Allocator, source: []const u8) BuiltinDocTestError!?[]u8 {
    return runEval(allocator, .expr, source);
}

/// Process a block.
///
/// Stage 1 (check) runs in the parent — it uses `parseAndCheckProgramForProblems`
/// which only canonicalizes and type-checks, paths that don't crash on
/// in-progress Builtin.roc code.
///
/// Stage 2 (test / eval) is isolated in a forked child so a single compiler
/// invariant panic on one block doesn't kill the rest of the run.
fn processBlock(allocator: Allocator, block: *const Block) BuiltinDocTestError!ProcessResult {
    if (try checkAccordingToKind(allocator, block)) |msg| return .{ .failed = msg };

    switch (block.kind) {
        .expects_only => return forkResultToProcess(allocator, runInChild(allocator, workerExpects, block.source)),
        .module_with_def => {
            const name = block.last_def_name orelse {
                return .{ .failed = try dupeErr(allocator, "module-style block has no top-level definition to use as main", .{}) };
            };
            const wrapped = try std.fmt.allocPrint(allocator, "{s}\n\nmain = {s}\n", .{ block.source, name });
            defer allocator.free(wrapped);
            return forkResultToProcess(allocator, runInChild(allocator, workerEvalModule, wrapped));
        },
        .expression_block => {
            const statements = try splitTopLevelStatements(allocator, block.source);
            defer allocator.free(statements);
            if (statements.len == 0) {
                return .{ .failed = try dupeErr(allocator, "expression block had no statements", .{}) };
            }
            for (statements, 0..) |stmt, i| {
                const outcome = runInChild(allocator, workerEvalExpr, stmt);
                switch (outcome) {
                    .success, .fork_unavailable => {},
                    .failed => |msg| {
                        defer allocator.free(msg);
                        return .{ .failed = try std.fmt.allocPrint(
                            allocator,
                            "expression {d} of {d} failed: {s}",
                            .{ i + 1, statements.len, msg },
                        ) };
                    },
                    .crashed => |sig| {
                        return .{ .crashed = sig };
                    },
                }
            }
            return .success;
        },
    }
}

fn forkResultToProcess(allocator: Allocator, outcome: ForkOutcome) BuiltinDocTestError!ProcessResult {
    return switch (outcome) {
        .success => .success,
        .failed => |msg| .{ .failed = msg },
        .crashed => |sig| .{ .crashed = sig },
        .fork_unavailable => .{ .failed = try dupeErr(allocator, "fork unavailable", .{}) },
    };
}

fn checkAccordingToKind(allocator: Allocator, block: *const Block) BuiltinDocTestError!?[]u8 {
    switch (block.kind) {
        .expects_only, .module_with_def => {
            return try runCheck(allocator, .module, block.source);
        },
        .expression_block => {
            // Each top-level expression is checked independently.
            const statements = try splitTopLevelStatements(allocator, block.source);
            defer allocator.free(statements);
            if (statements.len == 0) {
                return try dupeErr(allocator, "expression block had no statements", .{});
            }
            for (statements, 0..) |stmt, i| {
                if (try runCheck(allocator, .expr, stmt)) |msg| {
                    defer allocator.free(msg);
                    return try std.fmt.allocPrint(
                        allocator,
                        "expression {d} of {d} failed check: {s}",
                        .{ i + 1, statements.len, msg },
                    );
                }
            }
            return null;
        },
    }
}

/// Persist the failing block to disk and run the roc binary on it. Returns
/// true if the binary also reports a failure (matching our in-memory failure),
/// false if the binary succeeded (which would mean the test reported a false
/// positive).
fn reproduceWithBinary(
    allocator: Allocator,
    block: *const Block,
    block_index: usize,
) BuiltinDocTestError!bool {
    const wrapper = try wrapForBinary(allocator, block);
    defer allocator.free(wrapper);

    const path = try std.fmt.allocPrint(
        allocator,
        "{s}/builtin_doc_block_{d}_line_{d}.roc",
        .{ debug_dir, block_index, block.start_line },
    );
    defer allocator.free(path);

    // Ensure the debug directory exists.
    const ctx = CoreCtx.default(allocator, allocator, std.testing.io);
    ctx.makePath(debug_dir) catch {};

    try ctx.writeFile(path, wrapper);

    // Determine which roc subcommand reproduces the failure.
    const subcommand = switch (block.kind) {
        .expects_only => "test",
        .module_with_def, .expression_block => "check",
    };

    // Skip reproduction if the binary doesn't exist (e.g. tests run before build).
    const access_ctx = CoreCtx.default(allocator, allocator, std.testing.io);
    if (!access_ctx.fileExists(roc_binary)) {
        std.debug.print(
            "[builtin-doc-tests] roc binary not found at {s}; skipping reproduction step for block #{d} (line {d}). Debug file: {s}\n",
            .{ roc_binary, block_index, block.start_line, path },
        );
        return true;
    }

    var child = std.process.spawn(std.testing.io, .{
        .argv = &.{ roc_binary, subcommand, path, "--no-cache" },
        .stdout = .ignore,
        .stderr = .ignore,
    }) catch |err| {
        std.debug.print(
            "[builtin-doc-tests] could not spawn roc binary ({s}); leaving debug file at {s}\n",
            .{ @errorName(err), path },
        );
        return true;
    };
    const term = child.wait(std.testing.io) catch |err| {
        std.debug.print(
            "[builtin-doc-tests] roc binary wait failed ({s}); leaving debug file at {s}\n",
            .{ @errorName(err), path },
        );
        return true;
    };

    const binary_failed = switch (term) {
        .exited => |code| code != 0,
        else => true,
    };
    return binary_failed;
}

/// Wrap the block source so that the `roc` binary can run it standalone.
fn wrapForBinary(allocator: Allocator, block: *const Block) BuiltinDocTestError![]u8 {
    return switch (block.kind) {
        .expects_only => try std.fmt.allocPrint(allocator, "{s}\n", .{block.source}),
        .module_with_def => blk: {
            const name = block.last_def_name orelse return try std.fmt.allocPrint(
                allocator,
                "{s}\n",
                .{block.source},
            );
            break :blk try std.fmt.allocPrint(
                allocator,
                "{s}\n\nmain = {s}\n",
                .{ block.source, name },
            );
        },
        .expression_block => try std.fmt.allocPrint(allocator, "{s}\n", .{block.source}),
    };
}

test "Builtin.roc doc code blocks check and evaluate" {
    const allocator = base.defaultGpa();

    const ctx = CoreCtx.default(allocator, allocator, std.testing.io);
    const source = ctx.readFile(builtin_roc_path, allocator) catch |err| {
        std.debug.print(
            "[builtin-doc-tests] could not read {s} ({s}); tests must run from the project root.\n",
            .{ builtin_roc_path, @errorName(err) },
        );
        return err;
    };
    defer allocator.free(source);

    const blocks = try extractBlocks(allocator, source);
    defer {
        for (blocks) |*b| b.deinit(allocator);
        allocator.free(blocks);
    }

    try testing.expect(blocks.len > 0);

    // Load and publish the Builtin module once. Every block's check phase (in
    // this parent process) and eval phase (in a forked child, via copy-on-write)
    // borrows it read-only instead of re-creating the view and re-publishing it.
    var builtins_instance = try eval_mod.BuiltinModules.init(allocator);
    defer builtins_instance.deinit();
    shared_builtins = &builtins_instance;
    defer shared_builtins = null;

    var failures = std.ArrayList(Failure).empty;
    defer {
        for (failures.items) |*f| f.deinit(allocator);
        failures.deinit(allocator);
    }

    var phantom_failures: usize = 0;

    for (blocks, 0..) |*block, i| {
        if (containsEffectfulCall(block.source)) {
            continue;
        }
        const result = try processBlock(allocator, block);
        switch (result) {
            .success => {},
            .failed, .crashed => {
                const message = switch (result) {
                    .failed => |msg| msg,
                    .crashed => |sig| try std.fmt.allocPrint(allocator, "in-memory evaluation crashed with signal {d}", .{sig}),
                    else => unreachable,
                };
                const repro = reproduceWithBinary(allocator, block, i) catch |err| blk: {
                    std.debug.print(
                        "[builtin-doc-tests] reproduction failed with {s} (block #{d} line {d})\n",
                        .{ @errorName(err), i, block.start_line },
                    );
                    break :blk true;
                };
                if (!repro) {
                    phantom_failures += 1;
                    std.debug.print(
                        "[builtin-doc-tests] PHANTOM FAILURE: in-memory check reported a failure for block #{d} (Builtin.roc line {d}) but the roc binary succeeded — investigate!\nMessage was: {s}\n",
                        .{ i, block.start_line, message },
                    );
                }
                try failures.append(allocator, .{
                    .block_index = i,
                    .start_line = block.start_line,
                    .stage = @tagName(block.kind),
                    .message = message,
                    .crashed = result == .crashed,
                });
            },
        }
    }

    if (failures.items.len != 0) {
        var crashed_count: usize = 0;
        for (failures.items) |*f| if (f.crashed) {
            crashed_count += 1;
        };
        std.debug.print(
            "\n[builtin-doc-tests] {d} of {d} doc code blocks failed ({d} crashed):\n",
            .{ failures.items.len, blocks.len, crashed_count },
        );
        for (failures.items) |*f| {
            std.debug.print(
                "  - block #{d} (Builtin.roc line {d}, kind={s}){s}: {s}\n",
                .{ f.block_index, f.start_line, f.stage, if (f.crashed) " [CRASHED]" else "", f.message },
            );
        }
        std.debug.print(
            "\n[builtin-doc-tests] Failing blocks were written to {s}/builtin_doc_block_<N>_line_<L>.roc — open the corresponding file to debug a specific block.\n",
            .{debug_dir},
        );
        if (phantom_failures != 0) {
            std.debug.print(
                "[builtin-doc-tests] {d} of those failures could NOT be reproduced by the roc binary — the in-memory checker may disagree with the binary.\n",
                .{phantom_failures},
            );
            return error.PhantomFailuresDetected;
        }
        std.debug.print(
            "\n[builtin-doc-tests] FAIL: {d} doc code block(s) failed. Builtin.roc docs are gating tests; fix the blocks or update the docs so they do not claim runnable behavior.\n",
            .{failures.items.len},
        );
        return error.DocBlockFailures;
    }
}

/// Used by `runInChild EINTR regression` to mark that the test's signal
/// handler actually fired. File-scope because the C-ABI signal handler
/// cannot capture state any other way.
var eintr_test_signal_fired: std.atomic.Value(u32) = .init(0);

fn eintrTestSignalHandler(_: std.c.SIG) callconv(.c) void {
    _ = eintr_test_signal_fired.fetchAdd(1, .seq_cst);
}

/// Child-side work fn for the EINTR regression test. Sends SIGUSR1 to the
/// parent to interrupt its `waitpid`, then crashes via `abort()`. The parent
/// must report `.crashed` — without the EINTR retry, `waitpid` would return
/// -1 with `status` left at 0 and the harness would falsely report `.success`.
fn eintrTestChildWork(_: Allocator, _: []const u8) BuiltinDocTestError!?[]u8 {
    if (comptime !has_fork) return null;
    const fifty_ms: std.c.timespec = .{ .sec = 0, .nsec = 50 * std.time.ns_per_ms };
    // Give the parent a moment to enter waitpid before we interrupt it.
    _ = std.c.nanosleep(&fifty_ms, null);
    const ppid = std.c.getppid();
    _ = std.c.kill(ppid, .USR1);
    // Brief pause so the signal is delivered before we crash; then abort,
    // which terminates via SIGABRT so the parent sees `.crashed`.
    _ = std.c.nanosleep(&fifty_ms, null);
    std.c.abort();
}

test "runInChild retries waitpid on EINTR" {
    if (comptime !has_fork) return error.SkipZigTest;

    // Install a SIGUSR1 handler without SA_RESTART so the syscall is
    // interrupted (rather than auto-restarted by the kernel).
    var new_action: std.c.Sigaction = .{
        .handler = .{ .handler = eintrTestSignalHandler },
        .mask = std.mem.zeroes(std.c.sigset_t),
        .flags = 0,
    };
    var old_action: std.c.Sigaction = undefined;
    if (std.c.sigaction(.USR1, &new_action, &old_action) != 0) {
        return error.SigactionFailed;
    }
    defer _ = std.c.sigaction(.USR1, &old_action, null);

    eintr_test_signal_fired.store(0, .seq_cst);

    const outcome = runInChild(std.testing.allocator, eintrTestChildWork, "");
    // Free any owned message regardless of outcome shape.
    switch (outcome) {
        .failed => |msg| std.testing.allocator.free(msg),
        else => {},
    }

    // Sanity: the child actually managed to signal us (otherwise the EINTR
    // path was never exercised and this test is vacuous).
    try testing.expect(eintr_test_signal_fired.load(.seq_cst) >= 1);

    // The real assertion: the harness saw the crash, not a phantom success.
    try testing.expect(outcome == .crashed);
}
