//! Staging for Roc files that run on the built-in Echo platform.
//!
//! Two kinds of file get that platform: a headerless file with a `main!`
//! declaration, and an `app` header that names no platform. Both are compiled
//! as an app rooted at a staged copy that names the Echo platform written
//! beside it, so the rest of the pipeline sees an ordinary app.
//!
//! The staged copy keeps the user's source byte-for-byte after the wiring the
//! staging adds, and never adds or removes a line inside it, so a diagnostic's
//! line number in the staged copy maps back to the user's file by subtracting
//! `header_lines` and its byte offset by subtracting `header_len`.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;

/// The shorthand the staged app uses for the Echo platform. A platform-less
/// app header may already use this name for a package of its own, in which
/// case `stage` picks the next free `pf2`, `pf3`, ... instead.
const preferred_platform_alias = "pf";

/// Where the staged app finds the Echo platform, relative to the staged app
/// itself. Keeping this relative keeps a staged app's source identical from
/// run to run, which is what lets the checked-module cache hit across runs
/// even though the staging directory is new each time.
pub const platform_dir_name = ".roc_echo_platform";
const platform_main_spec = "./" ++ platform_dir_name ++ "/main.roc";

/// The wiring prepended to a headerless file: the app header naming the Echo
/// platform, and `echo!` bound to the platform's hosted function.
const headerless_wiring =
    "app [main!] { " ++ preferred_platform_alias ++ ": platform \"" ++ platform_main_spec ++ "\" }\n\n" ++
    "import " ++ preferred_platform_alias ++ ".Echo\n\n" ++
    "echo! = |msg| Echo.line!(msg)\n\n";

/// Which shape of source was staged.
pub const Kind = enum {
    /// A file with no header at all and a `main!` declaration.
    headerless,
    /// An `app` header that names packages but no platform.
    platformless_app,
};

/// How to classify a headerless file that failed to parse. A syntax error can
/// swallow every declaration after it, including `main!`, so such a file
/// cannot always be classified from the parsed declarations alone.
pub const UnparsableHeaderless = enum {
    /// Report it as not a default app. The plain-module paths used by `check`
    /// and `build` already report syntax errors against the user's file.
    not_default_app,
    /// Stage it as a default app. `run` needs the staged path so diagnostics
    /// are remapped to the user's file instead of reporting a missing header.
    default_app,
};

/// A staged default app: what the compiler compiles, plus what it takes to
/// map its diagnostics back to the file the user wrote.
pub const Staged = struct {
    kind: Kind,
    /// The user's source with line endings normalized. Owned.
    original_source: []const u8,
    /// The source the compiler compiles. Owned.
    synthetic_source: []const u8,
    /// Bytes the staging inserted before the user's body.
    header_len: usize,
    /// Lines the staging inserted before the user's body.
    header_lines: u32,

    pub fn deinit(self: *Staged, gpa: Allocator) void {
        gpa.free(self.original_source);
        gpa.free(self.synthetic_source);
        self.* = undefined;
    }
};

/// Errors staging can produce. A source that is neither headerless-with-`main!`
/// nor a platform-less app is not an error: `stage` returns null for it.
pub const Error = Allocator.Error;

/// Stage `source` (the bytes of a user's file) if it is a default app.
///
/// `source_dir` is the absolute directory the user's file lives in, which
/// anchors any relative package path the header declares: the staged copy
/// lives elsewhere, so those specs are rewritten to absolute paths as the copy
/// is built.
///
/// Takes ownership of nothing; the returned `Staged` owns its own buffers.
pub fn stage(
    gpa: Allocator,
    source_dir: []const u8,
    source: []const u8,
    unparsable: UnparsableHeaderless,
) Error!?Staged {
    const normalized = try base.source_utils.normalizeLineEndingsAlloc(gpa, source);
    const original_source = if (normalized.allocated) normalized.data else try gpa.dupe(u8, normalized.data);
    errdefer gpa.free(original_source);

    var env = try ModuleEnv.init(gpa, original_source);
    defer env.deinit();
    env.common.source = original_source;

    const ast = try parse.file(gpa, &env.common);
    defer ast.deinit();

    const file = ast.store.getFile();
    const header = ast.store.getHeader(file.header);

    switch (header) {
        .type_module => {
            if (!ast.hasMainBangDecl()) {
                const has_errors = ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0;
                if (unparsable != .default_app or !has_errors) {
                    gpa.free(original_source);
                    return null;
                }
            }
            const synthetic_source = try std.mem.concat(gpa, u8, &.{ headerless_wiring, original_source });
            return .{
                .kind = .headerless,
                .original_source = original_source,
                .synthetic_source = synthetic_source,
                .header_len = headerless_wiring.len,
                .header_lines = countNewlines(headerless_wiring),
            };
        },
        .app => |app| {
            if (app.platform_idx != null) {
                gpa.free(original_source);
                return null;
            }
            const staged = try stagePlatformlessApp(gpa, source_dir, original_source, ast, app);
            return staged;
        },
        .module, .package, .platform, .hosted, .default_app, .malformed => {
            gpa.free(original_source);
            return null;
        },
    }
}

/// Build the staged source for an app header that names no platform: the
/// user's source with the Echo platform spliced into its packages record and
/// the `echo!` binding appended.
///
/// The splice goes after the record's last entry, and the binding after the
/// last byte of the user's source, so every entry the user wrote keeps the
/// line and column they wrote it at, and every byte of the body moves by the
/// same amount.
fn stagePlatformlessApp(
    gpa: Allocator,
    source_dir: []const u8,
    original_source: []const u8,
    ast: *const parse.AST,
    app: @FieldType(parse.AST.Header, "app"),
) Error!Staged {
    const packages = ast.store.getCollection(app.packages);
    const fields = ast.store.recordFieldSlice(.{ .span = packages.span });

    var alias_buf: [preferred_platform_alias.len + 20]u8 = undefined;
    const alias = try platformAlias(ast, packages, &alias_buf);

    // Where the platform entry goes: after the last entry of the record, or
    // just past the opening `{` when the record has none.
    const splice_at: usize = if (fields.len == 0)
        ast.tokens.resolve(packages.region.start).end.offset
    else
        ast.tokens.resolve(ast.store.getRecordField(fields[fields.len - 1]).region.end - 1).end.offset;

    var synthetic = std.ArrayList(u8).empty;
    errdefer synthetic.deinit(gpa);

    // Copy the header, rewriting every relative package path to an absolute
    // one: the staged copy sits in a staging directory, so a path written
    // relative to the user's file would otherwise resolve there.
    var copied_through: usize = 0;
    for (fields) |field_idx| {
        // The compiler version pin shares the record with the dependencies
        // but is not one of them.
        if (app.roc_version) |roc_version_idx| {
            if (field_idx == roc_version_idx) continue;
        }

        const field = ast.store.getRecordField(field_idx);
        const value = field.value orelse continue;
        const spec_token = ast.store.singleStringPartToken(value) orelse continue;
        const spec_region = ast.tokens.resolve(spec_token);
        const spec = original_source[spec_region.start.offset..spec_region.end.offset];
        if (isUrlLike(spec) or std.fs.path.isAbsolute(spec)) continue;

        const absolute = try std.fs.path.resolve(gpa, &.{ source_dir, spec });
        defer gpa.free(absolute);

        try synthetic.appendSlice(gpa, original_source[copied_through..spec_region.start.offset]);
        try appendRocStringBody(gpa, &synthetic, absolute);
        copied_through = spec_region.end.offset;
    }
    try synthetic.appendSlice(gpa, original_source[copied_through..splice_at]);

    // Rewriting a path can shorten the header. Padding the platform entry
    // back out keeps the staged body at or past the offset the user's body is
    // at, which is what lets one number map a body offset back.
    if (synthetic.items.len < splice_at) {
        try synthetic.appendNTimes(gpa, ' ', splice_at - synthetic.items.len);
    }

    if (fields.len == 0) {
        try synthetic.print(gpa, " {s}: platform \"{s}\"", .{ alias, platform_main_spec });
    } else {
        try synthetic.print(gpa, ", {s}: platform \"{s}\"", .{ alias, platform_main_spec });
    }
    const header_len = synthetic.items.len - splice_at;

    try synthetic.appendSlice(gpa, original_source[splice_at..]);
    try synthetic.print(gpa, "\n\nimport {s}.Echo\n\necho! = |msg| Echo.line!(msg)\n", .{alias});

    return .{
        .kind = .platformless_app,
        .original_source = original_source,
        .synthetic_source = try synthetic.toOwnedSlice(gpa),
        .header_len = header_len,
        .header_lines = 0,
    };
}

/// The shorthand to give the Echo platform in a staged app: the preferred one,
/// or the first numbered variant of it that the header does not already use
/// for a package of its own.
fn platformAlias(
    ast: *const parse.AST,
    packages: parse.AST.Collection,
    buf: []u8,
) Allocator.Error![]const u8 {
    var suffix: usize = 1;
    while (true) : (suffix += 1) {
        const candidate = if (suffix == 1)
            preferred_platform_alias
        else
            // The buffer holds the alias plus more digits than a record with
            // one entry per distinct alias could ever need.
            std.fmt.bufPrint(buf, "{s}{d}", .{ preferred_platform_alias, suffix }) catch unreachable;

        var taken = false;
        for (ast.store.recordFieldSlice(.{ .span = packages.span })) |field_idx| {
            const field = ast.store.getRecordField(field_idx);
            if (std.mem.eql(u8, ast.resolve(field.name), candidate)) {
                taken = true;
                break;
            }
        }
        if (!taken) return candidate;
    }
}

/// Append `text` as the body of a Roc string literal, escaping the bytes that
/// a string literal cannot carry literally.
fn appendRocStringBody(gpa: Allocator, out: *std.ArrayList(u8), text: []const u8) Allocator.Error!void {
    for (text) |byte| {
        switch (byte) {
            '\\', '"', '$' => try out.append(gpa, '\\'),
            else => {},
        }
        try out.append(gpa, byte);
    }
}

/// Whether a package spec names a URL rather than a path. Mirrors the rule the
/// package resolver applies to the same spec text.
fn isUrlLike(spec: []const u8) bool {
    return std.mem.find(u8, spec, "://") != null;
}

/// Count the newlines in `text`.
fn countNewlines(text: []const u8) u32 {
    var count: u32 = 0;
    for (text) |c| {
        if (c == '\n') count += 1;
    }
    return count;
}

const testing = std.testing;

test "stage: a headerless file with main! gets the echo platform header" {
    var staged = (try stage(testing.allocator, "/tmp", "main! = |_| echo!(\"hi\")\n", .not_default_app)).?;
    defer staged.deinit(testing.allocator);

    try testing.expectEqual(Kind.headerless, staged.kind);
    try testing.expect(std.mem.startsWith(u8, staged.synthetic_source, headerless_wiring));
    try testing.expectEqual(headerless_wiring.len, staged.header_len);
}

test "stage: a file with no main! is not a default app" {
    try testing.expect(try stage(testing.allocator, "/tmp", "x = 1\n", .not_default_app) == null);
}

test "stage: an app header naming a platform is not a default app" {
    const source =
        \\app [main!] { pf: platform "../basic-cli/main.roc" }
        \\
        \\main! = |_| Ok({})
        \\
    ;
    try testing.expect(try stage(testing.allocator, "/tmp", source, .not_default_app) == null);
}

test "stage: a platform-less app header gets the echo platform and keeps its packages" {
    const source =
        \\app [main!] {
        \\    unicode: "https://example.com/unicode.tar.zst",
        \\}
        \\
        \\import unicode.Grapheme
        \\
        \\main! = |_| {
        \\    echo!("hi")
        \\    Ok({})
        \\}
        \\
    ;
    var staged = (try stage(testing.allocator, "/tmp", source, .not_default_app)).?;
    defer staged.deinit(testing.allocator);

    try testing.expectEqual(Kind.platformless_app, staged.kind);
    try testing.expect(std.mem.startsWith(
        u8,
        staged.synthetic_source,
        "app [main!] {\n    unicode: \"https://example.com/unicode.tar.zst\", pf: platform \"" ++ platform_main_spec ++ "\",\n}",
    ));
    // The echo! binding is appended.
    try testing.expect(std.mem.endsWith(u8, staged.synthetic_source, "import pf.Echo\n\necho! = |msg| Echo.line!(msg)\n"));

    // Every line of the user's source stays on the line the user wrote it on:
    // the only lines the staging adds are the appended `echo!` binding's.
    try testing.expectEqual(@as(u32, 0), staged.header_lines);
    try testing.expectEqual(countNewlines(source) + 5, countNewlines(staged.synthetic_source));
}

test "stage: a relative package path is rewritten for the staging directory" {
    const source =
        \\app [main!] { helper: "./helper/main.roc" }
        \\
        \\main! = |_| Ok({})
        \\
    ;
    var staged = (try stage(testing.allocator, "/home/user/proj", source, .not_default_app)).?;
    defer staged.deinit(testing.allocator);

    const expected_path = try std.fs.path.resolve(testing.allocator, &.{ "/home/user/proj", "helper/main.roc" });
    defer testing.allocator.free(expected_path);

    var expected_literal = std.ArrayList(u8).empty;
    defer expected_literal.deinit(testing.allocator);
    try expected_literal.appendSlice(testing.allocator, "helper: \"");
    try appendRocStringBody(testing.allocator, &expected_literal, expected_path);
    try expected_literal.append(testing.allocator, '"');

    try testing.expect(std.mem.find(u8, staged.synthetic_source, expected_literal.items) != null);
}

test "stage: the platform alias avoids a package that already uses it" {
    const source =
        \\app [main!] { pf: "https://example.com/pf.tar.zst" }
        \\
        \\main! = |_| Ok({})
        \\
    ;
    var staged = (try stage(testing.allocator, "/tmp", source, .not_default_app)).?;
    defer staged.deinit(testing.allocator);

    try testing.expect(std.mem.startsWith(u8, staged.synthetic_source, "app [main!] { pf: \"https://example.com/pf.tar.zst\", pf2: platform "));
    try testing.expect(std.mem.endsWith(u8, staged.synthetic_source, "import pf2.Echo\n\necho! = |msg| Echo.line!(msg)\n"));
}
