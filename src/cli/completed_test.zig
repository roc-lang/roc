//! Portable command records. Decoding allocates into a caller-owned arena;
//! no checked graph, lowering session or renderer state is persisted.
const std = @import("std");
const base = @import("base");
const reporting = @import("reporting");
const compile = @import("compile");
const Allocator = std.mem.Allocator;

/// Prefix of every command record; changing the record format changes it.
pub const magic = "ROC_COMPLETED_DEV_TEST_V3";
/// A SHA-256 content digest.
pub const Digest = [32]u8;
/// A file a producer read, with the content state it saw.
pub const Input = struct {
    path: []const u8,
    state: compile.watch_inputs.State,
};
/// A module path and the cwd-relative physical file the import resolver admitted for it.
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
/// publication side makes them relative; replay binds them to this invocation.
pub fn mapDiagnosticPaths(allocator: Allocator, diagnostic: *Diagnostic, cwd: []const u8, comptime restore: bool) Allocator.Error!void {
    if (diagnostic.file) |path| diagnostic.file = try mapPath(allocator, cwd, path, restore);
    try mapElements(allocator, diagnostic.headline, cwd, restore);
    try mapElements(allocator, diagnostic.body, cwd, restore);
}

fn mapPath(allocator: Allocator, cwd: []const u8, path: []const u8, comptime restore: bool) Allocator.Error![]const u8 {
    if (path.len == 0) return path;
    return if (restore) std.fs.path.resolve(allocator, &.{ cwd, path }) else std.fs.path.relative(allocator, cwd, null, cwd, path);
}

fn mapElements(allocator: Allocator, elements: []reporting.DocumentElement, cwd: []const u8, comptime restore: bool) Allocator.Error!void {
    for (elements) |*element| switch (element.*) {
        .annotated => |*a| if (a.annotation == .path) {
            a.content = try mapPath(allocator, cwd, a.content, restore);
        },
        .source_code_region => |*r| if (r.filename) |path| {
            r.filename = try mapPath(allocator, cwd, path, restore);
        },
        .source_code_multi_region => |*r| if (r.filename) |path| {
            r.filename = try mapPath(allocator, cwd, path, restore);
        },
        .source_code_with_underlines => |*r| if (r.display_region.filename) |path| {
            r.display_region.filename = try mapPath(allocator, cwd, path, restore);
        },
        .source_location => |*r| if (r.filename) |path| {
            r.filename = try mapPath(allocator, cwd, path, restore);
        },
        .vertical_stack, .horizontal_concat => |children| try mapElements(allocator, children, cwd, restore),
        .text, .line_break, .indent, .space, .horizontal_rule, .annotation_start, .annotation_end, .raw, .reflowing_text, .link => {},
    };
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
    try mapDiagnosticPaths(allocator, &portable, cwd, false);
    const portable_bytes = try encode(allocator, portable);
    try std.testing.expect(std.mem.find(u8, portable_bytes, cwd) == null);
    var restored = (try decode(allocator, Diagnostic, portable_bytes)).?;
    try mapDiagnosticPaths(allocator, &restored, cwd, true);
    var before = std.Io.Writer.Allocating.init(gpa);
    defer before.deinit();
    var after = std.Io.Writer.Allocating.init(gpa);
    defer after.deinit();
    const config = reporting.ReportingConfig.initForTesting();
    try original.render(gpa, &before.writer, config);
    try restored.render(gpa, &after.writer, config);
    try std.testing.expectEqualStrings(before.written(), after.written());
}
