//! Fuzzing for `roc fmt` comment preservation and stability.
//!
//! Each input generates a syntactically valid module, inserts comments between
//! its tokens, and checks that formatting keeps every comment, that the
//! formatted output parses, and that formatting it again changes nothing.
//!
//! To build just the repro executable:
//!   zig build build-repro-format
//!
//! To run one input:
//!   ./zig-out/bin/repro-format --verbose <seed_file>
//!
//! To run with AFL++:
//!   zig build -Dfuzz
//!   mkdir -p /tmp/roc-format-corpus
//!   printf '\0' > /tmp/roc-format-corpus/seed
//!   ./zig-out/AFLplusplus/bin/afl-fuzz -i /tmp/roc-format-corpus -o /tmp/roc-format-out zig-out/bin/fuzz-format

const std = @import("std");
const build_options = @import("build_options");
const can = @import("can");
const fmt = @import("fmt");
const parse = @import("parse");
const FuzzReader = @import("FuzzReader.zig");
const SyntaxGenerator = @import("SyntaxGenerator.zig");

const Token = parse.tokenize.Token;

pub export fn zig_fuzz_init() void {}

pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    var gpa_impl = std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    defer {
        _ = build_options.debugGpaOk(gpa_impl.deinit());
    }
    const gpa = gpa_impl.allocator();

    const input = buf[0..@intCast(len)];
    var reader = FuzzReader.init(input);

    var generator = SyntaxGenerator.init(gpa, &reader);
    defer generator.deinit();
    generator.generateModule() catch @panic("OOM during format fuzz source generation");
    const generated = generator.getOutput();
    if (debug) {
        std.debug.print("Input length: {d}, bytes consumed: {d}\n", .{ input.len, reader.position });
        std.debug.print("Generated:\n==========\n{s}\n==========\n\n", .{generated});
    }

    const commented = insertComments(gpa, generated, &reader) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM while inserting format fuzz comments"),
        error.ParseFailed => {
            std.debug.print("Generated:\n==========\n{s}\n==========\n\n", .{generated});
            @panic("format fuzz generator produced a module that does not parse");
        },
    };
    defer gpa.free(commented);
    if (debug) {
        std.debug.print("Commented:\n==========\n{s}\n==========\n\n", .{commented});
    }

    checkFormatting(gpa, generated, debug, .generated);
    checkFormatting(gpa, commented, debug, .commented);
}

const SourceKind = enum { generated, commented };

fn checkFormatting(gpa: std.mem.Allocator, source: []const u8, debug: bool, kind: SourceKind) void {
    const formatted = fmt.moduleFmtsStable(gpa, source, debug) catch |err| {
        switch (err) {
            // A line break where the module had none can change how it
            // parses, so inserting comments may produce invalid source.
            error.ParseFailed => if (kind == .commented) return,
            else => {},
        }
        if (!debug) std.debug.print("Source:\n==========\n{s}\n==========\n\n", .{source});
        std.debug.panic("formatting {s} source failed: {s}", .{ @tagName(kind), @errorName(err) });
    };
    gpa.free(formatted);
}

const comment_styles = [_][]const u8{
    " # c{d}\n",
    "\n# c{d}\n",
    "\n\n# c{d}\n",
    " #c{d}\n",
    "\n## c{d}\n",
};

/// Insert a comment between tokens wherever the reader asks for one. Comments
/// are numbered so that losing one cannot be hidden by another.
fn insertComments(gpa: std.mem.Allocator, source: []const u8, reader: *FuzzReader) error{ OutOfMemory, ParseFailed }![]u8 {
    var module_env = try can.ModuleEnv.init(gpa, source);
    defer module_env.deinit();
    const ast = try parse.file(gpa, &module_env.common);
    defer ast.deinit();
    if (ast.hasErrors()) {
        printFirstParseDiagnostic(ast, source);
        return error.ParseFailed;
    }

    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(gpa);
    const tags = ast.tokens.tokens.items(.tag);
    var copied: usize = 0;
    var comment_count: usize = 0;
    for (0..tags.len) |index| {
        if (index > 0 and insideString(tags[index - 1])) continue;
        const offset = if (index == 0) 0 else ast.tokens.resolve(index - 1).end.offset;
        // A line break where whitespace is significant usually changes how
        // the module parses, so comments go there less often. Exhausted input
        // reads as zeros, which comment every other boundary.
        const sensitive = whitespaceSensitive(if (index == 0) null else tags[index - 1], tags[index]);
        if (reader.intRangeLessThan(u8, 0, if (sensitive) 32 else 4) != @intFromBool(sensitive)) continue;
        try output.appendSlice(gpa, source[copied..offset]);
        copied = offset;
        switch (reader.intRangeAtMost(u8, 0, comment_styles.len)) {
            inline 0...comment_styles.len - 1 => |style| try output.print(gpa, comment_styles[style], .{comment_count}),
            comment_styles.len => try output.appendSlice(gpa, " #\n"),
            else => unreachable,
        }
        comment_count += 1;
    }
    try output.appendSlice(gpa, source[copied..]);
    return output.toOwnedSlice(gpa);
}

/// Whether whitespace between two tokens changes what they mean, as in `f(x)`
/// versus `f (x)`, or `-x` versus `- x`.
fn whitespaceSensitive(prev: ?Token.Tag, next: Token.Tag) bool {
    if (prev == .OpUnaryMinus) return true;
    return next == .NoSpaceOpenRound or
        next == .NoSpaceDotInt or
        next == .NoSpaceDotLowerIdent or
        next == .NoSpaceDotQuestionLowerIdent or
        next == .NoSpaceDotUpperIdent or
        next == .NoSpaceOpQuestion;
}

/// Whether the text right after a token of this kind is string content.
fn insideString(tag: Token.Tag) bool {
    return tag == .StringStart or tag == .MultilineStringStart or tag == .StringPart or tag == .CloseStringInterpolation;
}

fn printFirstParseDiagnostic(ast: *const parse.AST, source: []const u8) void {
    var first: ?parse.AST.Diagnostic = null;
    for (ast.parse_diagnostics.items) |diagnostic| {
        if (first == null or diagnostic.region.start < first.?.region.start) first = diagnostic;
    }
    if (first) |diagnostic| {
        const start = ast.tokens.resolve(diagnostic.region.start).start.offset;
        const line_start = if (std.mem.findScalarLast(u8, source[0..start], '\n')) |newline| newline + 1 else 0;
        std.debug.print("First parse diagnostic {s} at: {s}<HERE>{s}\n", .{ @tagName(diagnostic.tag), source[line_start..start], source[start..@min(source.len, start + 30)] });
    }
    for (ast.tokenize_diagnostics.items) |diagnostic| {
        std.debug.print("Tokenize diagnostic {s} at offset {d}\n", .{ @tagName(diagnostic.tag), diagnostic.region.start.offset });
    }
}
