//! Code tidiness checks for the Roc codebase.
//!
//! This file is adapted from TigerBeetle's tidy.zig with modifications for Roc.
//! Original source: https://github.com/tigerbeetle/tigerbeetle/blob/main/src/tidy.zig
//!
//! Copyright TigerBeetle, Inc.
//! Licensed under the Apache License, Version 2.0
//! See: https://www.apache.org/licenses/LICENSE-2.0
//!
//! Checks for various non-functional properties of the code itself.

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const fs = std.fs;
const mem = std.mem;
const Ast = std.zig.Ast;

const MiB = 1024 * 1024;

/// Binary file extensions that should be skipped entirely (not read into the buffer).
/// These are compiled artifacts, images, and other non-text files.
const binary_extensions: []const []const u8 = &.{
    ".ico", ".png", ".webp", ".jpg", ".jpeg", ".gif", ".bin",
    ".o", ".a", ".lib", ".dll", ".so", ".dylib", ".wasm",
    ".rlib", ".rmeta",
};

const TermColor = struct {
    pub const red = "\x1b[0;31m";
    pub const green = "\x1b[0;32m";
    pub const reset = "\x1b[0m";
};

pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var errors: Errors = .{};

    var counter: IdentifierCounter = try .init(gpa);
    defer counter.deinit(gpa);

    var dead_files_detector = DeadFilesDetector.init(gpa);
    defer dead_files_detector.deinit(gpa);

    // NB: all checks are intentionally implemented in a streaming fashion,
    // such that we only need to read the files once.
    const file_buffer = try gpa.alloc(u8, MiB + MiB / 2); // 1.5 MiB
    defer gpa.free(file_buffer);

    const paths = try listFilePaths(gpa);
    defer {
        for (paths) |path| {
            gpa.free(path);
        }
        gpa.free(paths);
    }

    for (paths) |file_path| {
        const bytes_read = (std.fs.cwd().readFile(file_path, file_buffer) catch |err| {
            std.debug.print("Error reading {s}: {}\n", .{ file_path, err });
            continue;
        }).len;
        if (bytes_read >= file_buffer.len - 1) {
            std.debug.panic(
                \\File exceeds 1.5 MiB buffer limit: {s}
                \\
                \\If this is a binary file, add its extension to `binary_extensions` in ci/tidy.zig
                \\to exclude it from tidy checks.
            , .{file_path});
        }
        file_buffer[bytes_read] = 0;

        const source_file = SourceFile{ .path = file_path, .text = file_buffer[0..bytes_read :0] };
        try tidyFile(gpa, &counter, source_file, &errors);

        if (source_file.hasExtension(".zig")) {
            try dead_files_detector.visit(source_file);
        }
    }

    dead_files_detector.finish(&errors);

    if (errors.count > 0) {
        std.debug.print("\n{s}[FAIL]{s} Found {d} tidy violations\n", .{ TermColor.red, TermColor.reset, errors.count });
        std.process.exit(1);
    }

    std.debug.print("{s}[OK]{s} All tidy checks passed!\n", .{ TermColor.green, TermColor.reset });
}

const Errors = struct {
    count: u32 = 0,

    pub fn addControlCharacter(
        errors: *Errors,
        file: SourceFile,
        offset: usize,
        character: u8,
    ) void {
        errors.emit(
            "{s}:{d}: error: control character code={}\n",
            .{ file.path, file.lineNumber(offset), character },
        );
    }

    pub fn addBanned(
        errors: *Errors,
        file: SourceFile,
        offset: usize,
        banned_item: []const u8,
        replacement: []const u8,
    ) void {
        errors.emit(
            "{s}:{d}: error: {s} is banned, use {s}\n",
            .{ file.path, file.lineNumber(offset), banned_item, replacement },
        );
    }

    pub fn addBannedReminder(
        errors: *Errors,
        file: SourceFile,
        offset: usize,
        banned_item: []const u8,
    ) void {
        errors.emit(
            "{s}:{d}: error: leftover {s}, remove before merge\n",
            .{ file.path, file.lineNumber(offset), banned_item },
        );
    }

    pub fn addAmbiguousPrecedence(errors: *Errors, file: SourceFile, line_index: usize) void {
        const line_number = line_index + 1;
        errors.emit(
            "{s}:{d}: error: ambiguous operator precedence, add parentheses\n",
            .{ file.path, line_number },
        );
    }

    pub fn addDeadDeclaration(errors: *Errors, file: SourceFile, declaration: []const u8) void {
        errors.emit("{s}: error: '{s}' is dead code\n", .{ file.path, declaration });
    }

    pub fn addInvalidMarkdownTitle(errors: *Errors, file: SourceFile) void {
        errors.emit(
            "{s}: error: document should have exactly one top-level '# Title'\n",
            .{file.path},
        );
    }

    pub fn addFileUntracked(errors: *Errors, file: []const u8) void {
        errors.emit(
            "{s}: error: imported file untracked by git\n",
            .{file},
        );
    }

    pub fn addFileDead(errors: *Errors, file: []const u8) void {
        errors.emit(
            "{s}: error: src/ file never imported in src/ or test/\n",
            .{file},
        );
    }

    fn emit(errors: *Errors, comptime fmt: []const u8, args: anytype) void {
        comptime assert(fmt[fmt.len - 1] == '\n');
        errors.count += 1;
        std.debug.print(fmt, args);
    }
};

const SourceFile = struct {
    path: []const u8,
    text: [:0]const u8,

    fn hasExtension(file: SourceFile, extension: []const u8) bool {
        assert(extension.len > 0);
        assert(extension[0] == '.');
        return std.mem.endsWith(u8, file.path, extension);
    }

    // O(1), but only invoked on the cold path (when there are errors).
    fn lineNumber(file: SourceFile, offset: usize) usize {
        assert(offset <= file.text.len);
        // +1: Line _index_ is zero-based, line _number_ is one-based.
        return std.mem.count(u8, file.text[0..offset], "\n") + 1;
    }
};

fn tidyFile(
    gpa: Allocator,
    counter: *IdentifierCounter,
    file: SourceFile,
    errors: *Errors,
) Allocator.Error!void {
    // Skip legacy crates/ directory entirely - it's scheduled for deletion
    if (std.mem.startsWith(u8, file.path, "crates/")) return;

    tidyControlCharacters(file, errors);
    if (file.hasExtension(".zig")) {
        tidyBanned(file, errors);

        var tree = try std.zig.Ast.parse(gpa, file.text, .zig);
        defer tree.deinit(gpa);

        tidyDeadDeclarations(file, &tree, counter, errors);
        tidyAst(file, &tree, errors);
    }
    if (file.hasExtension(".md")) {
        tidyMarkdownTitle(file, errors);
    }
}

fn tidyControlCharacters(file: SourceFile, errors: *Errors) void {
    // Check for carriage returns (CRLF line endings).
    // Tabs are intentionally allowed everywhere.
    // Windows batch files are allowed to have CRLF.
    if (file.hasExtension(".bat")) return;

    var remaining = file.text;
    while (mem.indexOfScalar(u8, remaining, '\r')) |index| {
        const offset = index + (file.text.len - remaining.len);
        errors.addControlCharacter(file, offset, '\r');
        remaining = remaining[index + 1 ..];
    }
}

fn tidyBanned(file: SourceFile, errors: *Errors) void {
    // Don't ban ourselves!
    if (std.mem.endsWith(u8, file.path, "ci/tidy.zig")) return;

    const ban_list: []const struct { []const u8, []const u8 } = &.{
        // Language footguns:
        .{ "== error.", "switch to avoid silent anyerror upcast" },
        .{ "!= error.", "switch to avoid silent anyerror upcast" },

        // Style:
        .{ "usingnamespace", "explicit imports" },
    };

    for (ban_list) |ban_item| {
        const banned, const replacement = ban_item;
        if (std.mem.indexOf(u8, file.text, banned)) |offset| {
            errors.addBanned(file, offset, banned, replacement);
        }
    }

    // Reminders:
    // Do use FIXME comments proactively while iterating on the code when you want to make sure
    // something is revisited before getting into the main branch.
    inline for (.{"FIXME"}) |banned| {
        if (std.mem.indexOf(u8, file.text, banned)) |offset| {
            errors.addBannedReminder(file, offset, banned);
        }
    }
}


const IdentifierCounter = struct {
    const file_identifier_count_max = 100_000;

    map: std.StringHashMapUnmanaged(struct { count: u32, offset: u32 }) = .{},

    pub fn init(gpa: Allocator) !IdentifierCounter {
        var counter: IdentifierCounter = .{};
        try counter.map.ensureTotalCapacity(gpa, file_identifier_count_max + 1);
        return counter;
    }

    pub fn deinit(counter: *IdentifierCounter, gpa: Allocator) void {
        counter.map.deinit(gpa);
        counter.* = undefined;
    }

    pub fn empty(counter: *const IdentifierCounter) bool {
        return counter.map.count() == 0;
    }

    pub fn clear(counter: *IdentifierCounter) void {
        counter.map.clearRetainingCapacity();
    }

    pub fn record(
        counter: *IdentifierCounter,
        tree: *const Ast,
        token_text: []const u8,
        token_offset: u32,
    ) void {
        const gop = counter.map.getOrPutAssumeCapacity(token_text);
        if (counter.map.count() > file_identifier_count_max) @panic("file too large");

        if (gop.found_existing) {
            // Count occurrences on a single line as one, as a special case for imports:
            // const foo = std.foo;
            const between_tokens_text = tree.source[gop.value_ptr.offset..token_offset];
            const same_line_occurrence = mem.indexOfScalar(u8, between_tokens_text, '\n') == null;
            if (same_line_occurrence) return;
        }

        if (!gop.found_existing) gop.value_ptr.* = .{ .count = 0, .offset = 0 };
        gop.value_ptr.count += 1;
        gop.value_ptr.offset = token_offset;
    }

    pub fn get(counter: *const IdentifierCounter, token_text: []const u8) u32 {
        return counter.map.get(token_text).?.count;
    }
};

/// Detects unused constants and functions.
///
/// This is a one-side heuristic: there might be false negatives, but no false positives.
///
/// Current algorithm:
/// - Two passes.
/// - Pass 1: count how many times each identifier is mentioned in the file.
/// - Pass 2: warn about any unique identifier which is a non-public declaration.
///
/// At the moment, this is implemented using only the lexer, without looking at the AST, as that
/// seemed simpler.
fn tidyDeadDeclarations(
    file: SourceFile,
    tree: *const Ast,
    counter: *IdentifierCounter,
    errors: *Errors,
) void {
    assert(counter.empty());
    defer counter.clear();

    var identifier_start: ?Ast.ByteOffset = 0;
    inline for (.{ .fill, .check }) |phase| {
        next_token: for (
            tree.tokens.items(.tag),
            tree.tokens.items(.start),
            0..,
        ) |tag, start, index_usize| {
            const index: Ast.TokenIndex = @intCast(index_usize);
            const identifier_start_previous = identifier_start;
            identifier_start = switch (tag) {
                .identifier => start,
                else => null,
            };

            const start_previous = identifier_start_previous orelse continue :next_token;
            const token_text = std.mem.trim(
                u8,
                tree.source[start_previous..start],
                &std.ascii.whitespace,
            );

            switch (phase) {
                .fill => counter.record(tree, token_text, start),
                .check => {
                    const usages = counter.get(token_text);
                    assert(usages >= 1);
                    if (usages == 1) {
                        if (tidyDeadDeclarationsIsPrivateDeclaration(tree, index - 1)) {
                            errors.addDeadDeclaration(file, token_text);
                        }
                    }
                },
                else => comptime unreachable,
            }
        }
    }
}

// Checks if the given identifier token refers to non-public declaration.
fn tidyDeadDeclarationsIsPrivateDeclaration(
    tree: *const Ast,
    token_index: Ast.TokenIndex,
) bool {
    assert(tree.tokens.items(.tag)[token_index] == .identifier);
    var declaration_keyword: ?std.zig.Token.Tag = null;
    var saw_extern = false;
    for (0..4) |context_offset| {
        const context_tag = if (token_index - context_offset < 1)
            .eof
        else
            tree.tokens.items(.tag)[token_index - context_offset - 1];

        if (declaration_keyword == null) {
            switch (context_tag) {
                .keyword_fn, .keyword_const => declaration_keyword = context_tag,
                // Not a declaration.
                else => return false,
            }
        } else {
            switch (context_tag) {
                .keyword_inline, .string_literal => {},
                .keyword_extern => saw_extern = true,
                // Public declaration can be used in a different file.
                .keyword_pub, .keyword_export => return false,
                // []const u8 or *const u8, not a declaration.
                .r_bracket, .asterisk => return false,
                // Non public declarations, never used.
                else => {
                    // Extern fn declarations are FFI bindings called by external code
                    if (saw_extern and declaration_keyword == .keyword_fn) {
                        return false;
                    }
                    return true;
                },
            }
        }
    } else unreachable;
}

fn tidyAst(
    file: SourceFile,
    tree: *const Ast,
    errors: *Errors,
) void {
    // Skip build files - they often have longer functions
    if (std.mem.eql(u8, file.path, "build.zig")) return;
    if (std.mem.endsWith(u8, file.path, "/build.zig")) return;

    const tags = tree.nodes.items(.tag);

    for (tags, 0..) |tag, node_usize| {
        const node: u32 = @intCast(node_usize);
        if (isBinOp(tag)) { // Forbid mixing bitops and arithmetics without parentheses.
            // In Zig 0.15, binary operations use node_and_node data layout
            const data = tree.nodeData(@enumFromInt(node));
            const children = data.node_and_node;
            inline for (children) |child| {
                const child_tag = tags[@intFromEnum(child)];
                if ((isBinOpBitwise(tag) and isBinOpArithmetic(child_tag)) or
                    (isBinOpArithmetic(tag) and isBinOpBitwise(child_tag)))
                {
                    const token_opening = tree.nodeMainToken(@enumFromInt(node));
                    const line_opening = tree.tokenLocation(0, token_opening).line;
                    errors.addAmbiguousPrecedence(file, line_opening);
                }
            }
        }
    }
}

fn isBinOp(tag: Ast.Node.Tag) bool {
    return isBinOpBitwise(tag) or isBinOpArithmetic(tag);
}

fn isBinOpBitwise(tag: Ast.Node.Tag) bool {
    return switch (tag) {
        .shl, .shl_sat => true,
        .shr => true,
        .bit_xor, .bit_or, .bit_and => true,
        else => false,
    };
}

fn isBinOpArithmetic(tag: Ast.Node.Tag) bool {
    return switch (tag) {
        .add, .add_sat, .add_wrap => true,
        .sub, .sub_sat, .sub_wrap => true,
        .mul, .mul_sat, .mul_wrap => true,
        .div, .mod => true,
        else => false,
    };
}

/// Checks that each markdown document has exactly one h1.
///
/// There are two schools of thought regarding largest (`# War and Peace`)
/// headings in markdown. One school says that they are _section_ titles, so
/// you could have multiple #'s in the document. But another option is to
/// say that a single # signifies document _title_, and there should be only
/// one in a document.
///
/// We use markdown to create HTML, so # turns into h1. MDN recommends that
/// there's only a single h1 in a page:
///
/// <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements#avoid_using_multiple_h1_elements_on_one_page>
///
/// For this reason, we follow the second convention.
fn tidyMarkdownTitle(file: SourceFile, errors: *Errors) void {
    // Skip directories with different conventions
    const skip_paths: []const []const u8 = &.{
        "test/snapshots/", // Snapshot files are generated
        "crates/",         // Old Rust crate code
        "design/",         // Design docs may have different structure
        "www/",            // Website content
    };
    for (skip_paths) |skip_path| {
        if (std.mem.indexOf(u8, file.path, skip_path) != null) return;
    }

    var fenced_block = false; // Avoid interpreting `# ` shell comments as titles.
    var heading_count: u32 = 0;
    var line_count: u32 = 0;
    var it = std.mem.splitScalar(u8, file.text, '\n');
    while (it.next()) |line| {
        line_count += 1;
        if (mem.startsWith(u8, line, "```")) fenced_block = !fenced_block;
        if (!fenced_block and mem.startsWith(u8, line, "# ")) heading_count += 1;
    }
    // Handle unclosed fenced blocks gracefully
    switch (heading_count) {
        // No need for a title for a short note.
        0 => if (line_count > 2) errors.addInvalidMarkdownTitle(file),
        1 => {},
        else => errors.addInvalidMarkdownTitle(file),
    }
}

// Zig's lazy compilation model makes it too easy to forget to include a file into the build --- if
// nothing imports a file, compiler just doesn't see it and can't flag it as unused.
//
// DeadFilesDetector implements heuristic detection of unused files, by "grepping" for import
// statements and flagging file which are never imported. This gives false negatives for unreachable
// cycles of files, as well as for identically-named files, but it should be good enough in
// practice.
const DeadFilesDetector = struct {
    const FileName = [64]u8;
    const FileState = struct { import_count: u32, definition_count: u32 };
    const FileMap = std.AutoArrayHashMap(FileName, FileState);

    files: FileMap,

    fn init(gpa: Allocator) DeadFilesDetector {
        return .{ .files = FileMap.init(gpa) };
    }

    fn deinit(detector: *DeadFilesDetector, _: Allocator) void {
        detector.files.deinit();
    }

    fn visit(detector: *DeadFilesDetector, file: SourceFile) Allocator.Error!void {
        assert(file.hasExtension(".zig"));

        // Only track src/ files as needing to be imported somewhere
        const is_src_file = std.mem.startsWith(u8, file.path, "src/");
        if (is_src_file) {
            (try detector.fileState(file.path)).definition_count += 1;
        }

        // Only scan src/, test/, and build files for imports
        const should_scan = is_src_file or
            std.mem.startsWith(u8, file.path, "test/") or
            std.mem.eql(u8, file.path, "build.zig") or
            std.mem.startsWith(u8, file.path, "ci/");
        if (!should_scan) return;

        var rest: []const u8 = file.text;
        for (0..1024) |_| {
            const result = cut(rest, "@import(\"") orelse break;
            rest = result[1];
            const result2 = cut(rest, "\")") orelse break;
            const import_path = result2[0];
            rest = result2[1];
            if (std.mem.endsWith(u8, import_path, ".zig")) {
                (try detector.fileState(import_path)).import_count += 1;
            }
        } else {
            std.debug.panic("file with more than 1024 imports: {s}", .{file.path});
        }
    }

    fn finish(detector: *DeadFilesDetector, errors: *Errors) void {
        defer detector.files.clearRetainingCapacity();

        for (detector.files.keys(), detector.files.values()) |name, state| {
            if (state.definition_count == 0) {
                errors.addFileUntracked(&name);
            }
            if (state.import_count == 0 and !isEntryPoint(name)) {
                errors.addFileDead(&name);
            }
        }
    }

    fn fileState(detector: *DeadFilesDetector, path: []const u8) !*FileState {
        const gop = try detector.files.getOrPut(pathToName(path));
        if (!gop.found_existing) gop.value_ptr.* = .{ .import_count = 0, .definition_count = 0 };
        return gop.value_ptr;
    }

    fn pathToName(path: []const u8) FileName {
        assert(std.mem.endsWith(u8, path, ".zig"));
        const basename = std.fs.path.basename(path);
        var file_name: FileName = @splat(0);
        assert(basename.len <= file_name.len);
        @memcpy(file_name[0..basename.len], basename);
        return file_name;
    }

    fn isEntryPoint(file: FileName) bool {
        // Entry points in src/ that are invoked directly (not via @import)
        const entry_points: []const []const u8 = &.{
            "main.zig", // CLI, playground_wasm, interpreter_shim, etc.
            "static_lib.zig", // Builtins static library
            "tracy.zig", // Profiler module (added via b.addModule)
            "tracy_stub.zig", // No-op tracy stub for standalone static library builds (added via b.addModule)
            "fuzz_sort.zig", // Fuzzing entry point
            "watch.zig", // File watcher entry point
            "fx_platform_test.zig", // FX platform tests
            "glue_test.zig", // Glue generation tests
            "host.zig", // Glue platform host
            "roc_subcommands.zig", // CLI subcommand tests
            "test_runner.zig", // Test runner executable
            "llvm_evaluator.zig", // LLVM evaluator executable
        };
        for (entry_points) |entry_point| {
            if (std.mem.startsWith(u8, &file, entry_point)) return true;
        }
        return false;
    }
};

/// Lists all files in the repository using git ls-files.
fn listFilePaths(allocator: Allocator) ![][]const u8 {
    var result = std.ArrayList([]const u8){};
    errdefer {
        for (result.items) |path| {
            allocator.free(path);
        }
        result.deinit(allocator);
    }

    var child = std.process.Child.init(&.{ "git", "ls-files", "-z" }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Ignore;

    _ = try child.spawn();

    const stdout = child.stdout orelse return error.NoStdout;
    const files = try stdout.readToEndAlloc(allocator, 10 * MiB);
    defer allocator.free(files);

    const term = try child.wait();
    if (term.Exited != 0) return error.GitFailed;

    if (files.len == 0) return result.toOwnedSlice(allocator);

    // git ls-files -z outputs null-separated paths
    var lines = std.mem.splitScalar(u8, files, 0);
    outer: while (lines.next()) |line| {
        if (line.len == 0) continue;
        // Skip binary files entirely - they shouldn't be read into the buffer
        for (binary_extensions) |ext| {
            if (std.mem.endsWith(u8, line, ext)) continue :outer;
        }
        try result.append(allocator, try allocator.dupe(u8, line));
    }

    return result.toOwnedSlice(allocator);
}

/// Splits a string at the first occurrence of a delimiter.
/// Returns null if delimiter is not found.
fn cut(str: []const u8, delimiter: []const u8) ?struct { []const u8, []const u8 } {
    const index = std.mem.indexOf(u8, str, delimiter) orelse return null;
    return .{ str[0..index], str[index + delimiter.len ..] };
}
