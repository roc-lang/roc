//! Portable command records. Decoding allocates into a caller-owned arena;
//! no checked graph, lowering session or renderer state is persisted.
const std = @import("std");
const base = @import("base");
const reporting = @import("reporting");
const compile = @import("compile");
const Allocator = std.mem.Allocator;

/// Prefix of every command record; changing the record format changes it.
pub const magic = "ROC_COMPLETED_DEV_TEST_V4";
/// A SHA-256 content digest.
pub const Digest = [32]u8;
/// Binds record paths to one invocation. A record never names the checkout,
/// the package cache or a staging directory: a file of a package whose root
/// is placed by the compiler is named by that package, and any other file
/// relative to the directory of the file under test.
pub const Paths = struct {
    /// Resolves a relative path the compiler recorded.
    cwd: []const u8,
    /// The directory of the file under test, which a project moves with.
    base: []const u8,
    placed: []const Placed,

    /// A package whose name identifies it wherever its root directory lands.
    pub const Placed = struct { name: []const u8, root_dir: []const u8 };

    /// Separates a placed package's name from a path inside it. Neither a
    /// package name nor a file path can contain it.
    const separator = 0;

    /// The name of absolute `path` in a record.
    pub fn portable(self: Paths, allocator: Allocator, path: []const u8) Allocator.Error![]const u8 {
        var owner: ?Placed = null;
        for (self.placed) |package| {
            if (!isInside(package.root_dir, path)) continue;
            if (owner == null or package.root_dir.len > owner.?.root_dir.len) owner = package;
        }
        const package = owner orelse return std.fs.path.relative(allocator, self.cwd, null, self.base, path);
        const inner = try std.fs.path.relative(allocator, self.cwd, null, package.root_dir, path);
        return std.fmt.allocPrint(allocator, "{s}{c}{s}", .{ package.name, separator, inner });
    }

    /// The absolute path a record's `path` names in this invocation, or null
    /// when it names a placed package this invocation does not have.
    pub fn bind(self: Paths, allocator: Allocator, path: []const u8) Allocator.Error!?[]const u8 {
        const split = std.mem.findScalar(u8, path, separator) orelse
            return try std.fs.path.resolve(allocator, &.{ self.base, path });
        for (self.placed) |package| {
            if (std.mem.eql(u8, package.name, path[0..split])) return try std.fs.path.resolve(allocator, &.{ package.root_dir, path[split + 1 ..] });
        }
        return null;
    }

    fn isInside(dir: []const u8, path: []const u8) bool {
        return path.len > dir.len and std.mem.startsWith(u8, path, dir) and std.fs.path.isSep(path[dir.len]);
    }
};

/// A file a producer read, with the content state it saw.
pub const Input = struct {
    path: []const u8,
    state: compile.watch_inputs.State,
};
/// A module path and the physical file the import resolver admitted for it.
pub const Resolution = struct { path: []const u8, target: []const u8 };
/// Everything a command's outcome depends on outside the compiler, in sorted order.
pub const Plan = struct { inputs: []Input, resolutions: []Resolution };
/// Producer-authored location-independent reporting identity and current region.
pub const ReportSite = @import("check").CheckedArtifact.TestReportSite;
/// A source owner and its reporting map under the exact command input manifest.
pub const Source = struct { name: Digest, qualified_name: []const u8, path: []const u8, sites: []const ReportSite };
/// A current test occurrence and the durable semantic outcome it instantiates.
pub const Root = struct { order: u32, region: base.Region, key: Digest, payload_hash: Digest };
/// A tested module and its current root occurrences.
pub const Module = struct {
    source: u32,
    roots: []const Root,
};
/// A non-error report from compilation, stored as its structured document content.
pub const Diagnostic = struct {
    title: []const u8,
    severity: reporting.Severity,
    file: ?[]const u8,
    headline: []reporting.DocumentElement,
    body: []reporting.DocumentElement,

    /// Borrow a report's content; the caller keeps the report alive until encoding.
    pub fn fromReport(report: *const reporting.Report) Diagnostic {
        return .{ .title = report.title, .severity = report.severity, .file = report.file, .headline = report.headline.elements.items, .body = report.document.elements.items };
    }

    /// Render the stored report with the current terminal configuration.
    pub fn render(self: Diagnostic, allocator: Allocator, writer: *std.Io.Writer, config: reporting.ReportingConfig) (Allocator.Error || std.Io.Writer.Error)!void {
        // The record arena owns all document content. Rendering borrows it.
        var report = reporting.Report{
            .title = self.title,
            .severity = self.severity,
            .file = self.file,
            .headline = reporting.Document.init(allocator),
            .document = reporting.Document.init(allocator),
            .allocator = allocator,
        };
        report.headline.elements.items = self.headline;
        report.document.elements.items = self.body;
        try reporting.renderReportToTerminal(&report, writer, reporting.ColorUtils.getPaletteForConfig(config), config);
    }
};
/// A complete test command's outcome, valid only for the plan whose key is `input_key`.
pub const Record = struct {
    input_key: Digest,
    sources: []const Source,
    modules: []const Module,
    debug_messages: []const []const u8,
    diagnostics: []Diagnostic,
    modules_with_tests: u32,
};

/// The digest used for every input, payload and record in the command cache.
pub fn hash(bytes: []const u8) Digest {
    return compile.watch_inputs.hashBytes(bytes);
}

/// Every field is explicitly encoded, without native padding, pointer values
/// or host endianness. This codec is private to the versioned command format.
pub fn encode(allocator: Allocator, value: anytype) Allocator.Error![]u8 {
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, magic);
    try put(allocator, &bytes, value);
    const digest = hash(bytes.items);
    try bytes.appendSlice(allocator, &digest);
    return bytes.toOwnedSlice(allocator);
}

fn put(allocator: Allocator, bytes: *std.ArrayList(u8), value: anytype) Allocator.Error!void {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .void => {},
        .bool => try bytes.append(allocator, @intFromBool(value)),
        .int => {
            var buf: [@sizeOf(T)]u8 = undefined;
            std.mem.writeInt(T, &buf, value, .little);
            try bytes.appendSlice(allocator, &buf);
        },
        .@"enum" => try put(allocator, bytes, @as(u32, @intCast(@backingInt(value)))),
        .optional => {
            try put(allocator, bytes, value != null);
            if (value) |v| try put(allocator, bytes, v);
        },
        .array => for (value) |v| {
            try put(allocator, bytes, v);
        },
        .pointer => |p| {
            if (p.size != .slice) @compileError("command records contain slices, never pointers");
            try put(allocator, bytes, std.math.cast(u32, value.len) orelse return error.OutOfMemory);
            for (value) |v| try put(allocator, bytes, v);
        },
        .@"struct" => |s| inline for (s.field_names) |field| {
            try put(allocator, bytes, @field(value, field));
        },
        .@"union" => |u| {
            const tag = std.meta.activeTag(value);
            try put(allocator, bytes, tag);
            inline for (u.field_names) |field| {
                if (std.mem.eql(u8, @tagName(tag), field)) return put(allocator, bytes, @field(value, field));
            }
            unreachable; // the active tag names one of the union's declared fields
        },
        .type, .noreturn, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported command record field " ++ @typeName(T)),
    }
}

const DecodeError = Allocator.Error || error{InvalidRecord};
const Reader = struct {
    allocator: Allocator,
    bytes: []const u8,

    fn take(self: *Reader, count: usize) error{InvalidRecord}![]const u8 {
        if (count > self.bytes.len) return error.InvalidRecord;
        const result = self.bytes[0..count];
        self.bytes = self.bytes[count..];
        return result;
    }

    fn get(self: *Reader, comptime T: type) DecodeError!T {
        return switch (@typeInfo(T)) {
            .void => {},
            .bool => switch ((try self.take(1))[0]) {
                0 => false,
                1 => true,
                else => error.InvalidRecord,
            },
            .int => std.mem.readInt(T, (try self.take(@sizeOf(T)))[0..@sizeOf(T)], .little),
            .@"enum" => std.enums.fromInt(T, try self.get(u32)) orelse error.InvalidRecord,
            .optional => |o| if (try self.get(bool)) try self.get(o.child) else null,
            .array => |a| blk: {
                var result: T = undefined;
                for (&result) |*v| v.* = try self.get(a.child);
                break :blk result;
            },
            .pointer => |p| blk: {
                const count = try self.get(u32);
                // Every element of a persisted slice consumes at least a byte.
                if (count > self.bytes.len) return error.InvalidRecord;
                const result = try self.allocator.alloc(p.child, count);
                for (result) |*v| v.* = try self.get(p.child);
                break :blk result;
            },
            .@"struct" => |s| blk: {
                var result: T = undefined;
                inline for (s.field_names) |field| @field(result, field) = try self.get(@FieldType(T, field));
                break :blk result;
            },
            .@"union" => |u| blk: {
                const tag = try self.get(u.tag_type.?);
                inline for (u.field_names) |field| {
                    if (std.mem.eql(u8, @tagName(tag), field)) break :blk @unionInit(T, field, try self.get(@FieldType(T, field)));
                }
                unreachable; // the decoded union tag names one of its declared fields
            },
            .type, .noreturn, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported command record field " ++ @typeName(T)),
        };
    }
};

/// Decode a checksummed record into `allocator`; a damaged or foreign record is null.
pub fn decode(allocator: Allocator, comptime T: type, bytes: []const u8) Allocator.Error!?T {
    if (bytes.len < magic.len + 32 or !std.mem.startsWith(u8, bytes, magic)) return null;
    const body = bytes[0 .. bytes.len - 32];
    if (!std.mem.eql(u8, &hash(body), bytes[bytes.len - 32 ..])) return null;
    var reader = Reader{ .allocator = allocator, .bytes = body[magic.len..] };
    const result = reader.get(T) catch |err| switch (err) {
        error.InvalidRecord => return null,
        error.OutOfMemory => return error.OutOfMemory,
    };
    if (reader.bytes.len != 0) return null;
    return result;
}

/// Paths in source locations and path annotations remain structured. The
/// publication side makes them portable; replay binds them to this invocation
/// and answers false when one names a package the invocation does not have.
pub fn mapDiagnosticPaths(allocator: Allocator, diagnostic: *Diagnostic, paths: Paths, comptime restore: bool) Allocator.Error!bool {
    if (diagnostic.file) |path| diagnostic.file = (try mapPath(allocator, paths, path, restore)) orelse return false;
    return try mapElements(allocator, diagnostic.headline, paths, restore) and try mapElements(allocator, diagnostic.body, paths, restore);
}

fn mapPath(allocator: Allocator, paths: Paths, path: []const u8, comptime restore: bool) Allocator.Error!?[]const u8 {
    if (path.len == 0) return path;
    return if (restore) paths.bind(allocator, path) else try paths.portable(allocator, path);
}

fn mapElements(allocator: Allocator, elements: []reporting.DocumentElement, paths: Paths, comptime restore: bool) Allocator.Error!bool {
    for (elements) |*element| switch (element.*) {
        .annotated => |*a| if (a.annotation == .path) {
            a.content = (try mapPath(allocator, paths, a.content, restore)) orelse return false;
        },
        .source_code_region => |*r| if (r.filename) |path| {
            r.filename = (try mapPath(allocator, paths, path, restore)) orelse return false;
        },
        .source_code_multi_region => |*r| if (r.filename) |path| {
            r.filename = (try mapPath(allocator, paths, path, restore)) orelse return false;
        },
        .source_code_with_underlines => |*r| if (r.display_region.filename) |path| {
            r.display_region.filename = (try mapPath(allocator, paths, path, restore)) orelse return false;
        },
        .source_location => |*r| if (r.filename) |path| {
            r.filename = (try mapPath(allocator, paths, path, restore)) orelse return false;
        },
        .vertical_stack, .horizontal_concat => |children| if (!try mapElements(allocator, children, paths, restore)) return false,
        .text, .line_break, .indent, .space, .horizontal_rule, .annotation_start, .annotation_end, .raw, .reflowing_text, .link => {},
    };
    return true;
}

test "record paths name placed packages, never the checkout or cache root" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const first = Paths{ .cwd = "/", .base = "/checkout/a", .placed = &.{.{ .name = "https://example.com/pkg.tar.zst", .root_dir = "/cache/one/pkg" }} };
    const second = Paths{ .cwd = "/other", .base = "/elsewhere/deep/b", .placed = &.{.{ .name = "https://example.com/pkg.tar.zst", .root_dir = "/other/cache/pkg" }} };
    const local = try first.portable(allocator, "/checkout/a/src/Main.roc");
    try std.testing.expectEqualStrings(local, try second.portable(allocator, "/elsewhere/deep/b/src/Main.roc"));
    try std.testing.expectEqualStrings("/elsewhere/deep/b/src/Main.roc", (try second.bind(allocator, local)).?);
    const fetched = try first.portable(allocator, "/cache/one/pkg/Lib.roc");
    try std.testing.expectEqualStrings(fetched, try second.portable(allocator, "/other/cache/pkg/Lib.roc"));
    try std.testing.expectEqualStrings("/other/cache/pkg/Lib.roc", (try second.bind(allocator, fetched)).?);
    // A directory that only shares a name prefix is not inside the package.
    try std.testing.expectEqualStrings("../../cache/one/pkg2/Lib.roc", try first.portable(allocator, "/cache/one/pkg2/Lib.roc"));
    try std.testing.expect((try (Paths{ .cwd = "/", .base = "/checkout/a", .placed = &.{} }).bind(allocator, fetched)) == null);
}

test "completed test records reject every truncated prefix and corrupted content" {
    const gpa = std.testing.allocator;
    const bytes = try encode(gpa, @as([]const Input, &.{.{ .path = "missing.txt", .state = .missing }}));
    defer gpa.free(bytes);
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    for (0..bytes.len) |len| try std.testing.expect((try decode(arena.allocator(), []const Input, bytes[0..len])) == null);
    const loaded = (try decode(arena.allocator(), []const Input, bytes)).?;
    try std.testing.expectEqualStrings("missing.txt", loaded[0].path);
    bytes[magic.len + 4] ^= 1;
    try std.testing.expect((try decode(arena.allocator(), []const Input, bytes)) == null);

    // Even a checksum-valid malformed length must fail ordinary decoding.
    @memset(bytes[magic.len..][0..4], 0xff);
    @memcpy(bytes[bytes.len - 32 ..], &hash(bytes[0 .. bytes.len - 32]));
    try std.testing.expect((try decode(arena.allocator(), []const Input, bytes)) == null);
}

test "completed test diagnostics retain structured source and styling across path rebinding" {
    const gpa = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const allocator = arena.allocator();
    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(std.testing.io, ".", allocator);
    const path = try std.fs.path.join(allocator, &.{ cwd, "Example.roc" });
    var report = try reporting.Report.init(gpa, "Unused Variable", "This binding is unused.", .warning);
    defer report.deinit();
    try report.document.addAnnotated(path, .path);
    try report.document.addLineBreak();
    try report.addSourceContext(.{ .start_line_idx = 0, .end_line_idx = 0, .start_col_idx = 0, .end_col_idx = 6 }, path, "unused = 1\n", &.{0});
    const original = Diagnostic.fromReport(&report);
    const cloned_bytes = try encode(allocator, original);
    var portable = (try decode(allocator, Diagnostic, cloned_bytes)).?;
    const paths = Paths{ .cwd = cwd, .base = cwd, .placed = &.{} };
    try std.testing.expect(try mapDiagnosticPaths(allocator, &portable, paths, false));
    const portable_bytes = try encode(allocator, portable);
    try std.testing.expect(std.mem.find(u8, portable_bytes, cwd) == null);
    var restored = (try decode(allocator, Diagnostic, portable_bytes)).?;
    try std.testing.expect(try mapDiagnosticPaths(allocator, &restored, paths, true));
    var before = std.Io.Writer.Allocating.init(gpa);
    defer before.deinit();
    var after = std.Io.Writer.Allocating.init(gpa);
    defer after.deinit();
    const config = reporting.ReportingConfig.initForTesting();
    try original.render(gpa, &before.writer, config);
    try restored.render(gpa, &after.writer, config);
    try std.testing.expectEqualStrings(before.written(), after.written());
}
