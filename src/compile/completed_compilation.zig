//! A completed compilation is an alternative to a live evaluation session.
//! Checked input identities are validated before consuming this outcome; no
//! checked module is marked evaluated by replaying it.
const std = @import("std");
const reporting = @import("reporting");
const Allocator = std.mem.Allocator;

/// A module of the completed compilation, named by its checked identity.
pub const Module = struct {
    key: [32]u8,
    path: []const u8,
    identity_path: []const u8 = "",
};

/// Compiler-owned staging paths are relative to an explicitly named package.
/// Ordinary source paths retain the spelling used by the compilation.
pub const InputPath = struct {
    package: ?[]const u8 = null,
    path: []const u8,
};
/// A source or asset and the content state consumed by compilation.
pub const FileInput = struct { path: InputPath, state: @import("watch_inputs.zig").State };
/// A source path and its canonical identity accepted during import resolution.
pub const PathCheck = struct { path: InputPath, canonical: InputPath };
/// Exact directory-entry spellings accepted by import resolution. Unrelated
/// files and build outputs in the same directory are not compilation inputs.
pub const DirectoryInput = struct { path: InputPath, entries: []const InputPath };
/// Inputs sufficient to validate a completed compilation without checked IR.
pub const Inputs = struct {
    resolution: [32]u8,
    files: []const FileInput,
    path_checks: []const PathCheck,
    directories: []const DirectoryInput,
    /// Same order as Outcome.modules, for diagnostic path rebinding.
    module_paths: []const InputPath,
};

/// Capture documents before their rendering owner releases them. The returned
/// record is independent of both checked IR and the report's arena.
pub fn copyDiagnostic(arena: Allocator, report: *const reporting.Report) Allocator.Error!Diagnostic {
    const bytes = try encode(arena, Diagnostic{
        .module = @splat(0),
        .title = report.title,
        .severity = report.severity,
        .file = report.file,
        .headline = report.headline.elements.items,
        .body = report.document.elements.items,
    });
    var reader = Reader{ .bytes = bytes, .arena = arena, .remaining = bytes.len * 4 };
    return reader.read(Diagnostic) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidRecord => @import("base").invariant("completion diagnostic writer produced invalid bytes", .{}),
    };
}

/// A report a module produced, kept as typed document items so its paths
/// can be rebound to the consuming invocation's source paths.
pub const Diagnostic = struct {
    module: [32]u8,
    title: []const u8,
    severity: reporting.Severity,
    file: ?[]const u8,
    headline: []reporting.DocumentElement,
    body: []reporting.DocumentElement,
};

/// Checked identities survive arena teardown and are independent of the
/// producer's transient Monotype/Solved root positions.
pub const Root = struct { module: [32]u8, root: u32 };
/// A compile-time root the compilation evaluated and the roots it read.
pub const RootDemand = struct { root: Root, reads: []const Root };
/// A literal checked at compile time and the roots its check read.
pub const LiteralDemand = struct {
    module: [32]u8,
    checked_expr: u32,
    kind: @import("lir").LIR.LiteralRejectionKind,
    identity: ?[32]u8,
    reads: []const Root,
};
/// The compile-time demand one compile-time evaluation recorded.
pub const Demand = struct {
    evaluated: []const RootDemand,
    stored: []const Root,
    literals: []const LiteralDemand,
};

/// Everything a completed compilation records for a later invocation to
/// consume in place of evaluating its modules again.
pub const Outcome = struct {
    modules: []const Module = &.{},
    diagnostics: []const Diagnostic = &.{},
    observations: []const []const u8 = &.{},
    demand: []const Demand = &.{},
    targets: ?@import("targets_config.zig").TargetsConfig = null,
};

/// Small, versioned records use length-prefixed slices, explicit union tags,
/// and little-endian integers. No padding, pointers or allocator state is
/// persisted. Callers own an arena for decoded slices.
pub fn encode(allocator: Allocator, value: anytype) Allocator.Error![]u8 {
    var out: std.Io.Writer.Allocating = .init(allocator);
    defer out.deinit();
    write(@TypeOf(value), &out.writer, value) catch return error.OutOfMemory;
    return out.toOwnedSlice();
}

/// Appends the encoding of one record value to `out`.
pub fn write(comptime T: type, out: *std.Io.Writer, value: T) std.Io.Writer.Error!void {
    switch (@typeInfo(T)) {
        .void => {},
        .bool => try out.writeByte(@intFromBool(value)),
        .int => try out.writeInt(T, value, .little),
        .@"enum" => try out.writeInt(u64, @backingInt(value), .little),
        .optional => |info| {
            try out.writeByte(@intFromBool(value != null));
            if (value) |v| try write(info.child, out, v);
        },
        .array => |info| for (value) |v| try write(info.child, out, v),
        .pointer => |info| {
            if (info.size != .slice) @compileError("record contains a process pointer");
            try out.writeInt(u64, value.len, .little);
            if (info.child == u8) try out.writeAll(value) else for (value) |v| try write(info.child, out, v);
        },
        .@"struct" => |info| inline for (info.field_names, info.field_types) |name, Field| try write(Field, out, @field(value, name)),
        .@"union" => |info| {
            const tag = std.meta.activeTag(value);
            try write(info.tag_type.?, out, tag);
            inline for (info.field_names, info.field_types) |name, Field| {
                if (tag == @field(info.tag_type.?, name)) try write(Field, out, @field(value, name));
            }
        },
        .type, .noreturn, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported record field: " ++ @typeName(T)),
    }
}

/// Decodes records written by `write`, rejecting malformed input.
pub const Reader = struct {
    bytes: []const u8,
    arena: Allocator,
    offset: usize = 0,
    /// Each decoded node consumes at least one unit, bounding allocations and
    /// recursion by the validated record's size rather than untrusted counts.
    remaining: usize,

    pub const Error = Allocator.Error || error{InvalidRecord};

    pub fn take(self: *Reader, len: usize) Error![]const u8 {
        if (len > self.bytes.len - self.offset) return error.InvalidRecord;
        defer self.offset += len;
        return self.bytes[self.offset..][0..len];
    }

    pub fn read(self: *Reader, comptime T: type) Error!T {
        if (self.remaining == 0) return error.InvalidRecord;
        self.remaining -= 1;
        switch (@typeInfo(T)) {
            .void => return {},
            .bool => return switch ((try self.take(1))[0]) {
                0 => false,
                1 => true,
                else => error.InvalidRecord,
            },
            .int => return std.mem.readInt(T, (try self.take(@sizeOf(T)))[0..@sizeOf(T)], .little),
            .@"enum" => {
                const value = try self.read(u64);
                return std.enums.fromInt(T, value) orelse return error.InvalidRecord;
            },
            .optional => |info| return if (try self.read(bool)) try self.read(info.child) else null,
            .array => |info| {
                var result: T = undefined;
                for (&result) |*v| v.* = try self.read(info.child);
                return result;
            },
            .pointer => |info| {
                if (info.size != .slice) @compileError("record contains a process pointer");
                const count = std.math.cast(usize, try self.read(u64)) orelse return error.InvalidRecord;
                if (info.child == u8) {
                    const bytes = try self.take(count);
                    return if (info.attrs.@"const") bytes else try self.arena.dupe(u8, bytes);
                }
                if (count > self.remaining) return error.InvalidRecord;
                const result = try self.arena.alloc(info.child, count);
                for (result) |*v| v.* = try self.read(info.child);
                return result;
            },
            .@"struct" => |info| {
                var result: T = undefined;
                inline for (info.field_names, info.field_types) |name, Field| @field(result, name) = try self.read(Field);
                return result;
            },
            .@"union" => |info| {
                const tag = try self.read(info.tag_type.?);
                inline for (info.field_names, info.field_types) |name, Field| {
                    if (@backingInt(tag) == @backingInt(@field(info.tag_type.?, name)))
                        return @unionInit(T, name, try self.read(Field));
                }
                return error.InvalidRecord;
            },
            .type, .noreturn, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported record field: " ++ @typeName(T)),
        }
    }
};

/// Rebind typed paths to this invocation's source paths. Prose and dbg text
/// remain language observations, never terminal-rendered output.
pub fn rebindElements(elements: []reporting.DocumentElement, before: []const Module, after: []const Module) void {
    for (elements) |*element| switch (element.*) {
        .source_location => |*v| v.filename = rebindPath(v.filename, before, after),
        .source_code_region => |*v| v.filename = rebindPath(v.filename, before, after),
        .source_code_multi_region => |*v| v.filename = rebindPath(v.filename, before, after),
        .source_code_with_underlines => |*v| v.display_region.filename = rebindPath(v.display_region.filename, before, after),
        .annotated => |*v| if (v.annotation == .path) {
            v.content = rebindPath(v.content, before, after).?;
        },
        .vertical_stack, .horizontal_concat => |v| rebindElements(v, before, after),
        .text, .line_break, .indent, .space, .horizontal_rule, .annotation_start, .annotation_end, .raw, .reflowing_text, .link => {},
    };
}

/// The source path in `after` of the module `path` named in `before`, or
/// `path` itself when no module of `before` has it.
pub fn rebindPath(path: ?[]const u8, before: []const Module, after: []const Module) ?[]const u8 {
    const value = path orelse return null;
    for (before) |old| {
        if (!std.mem.eql(u8, old.path, value)) continue;
        for (after) |new| if (std.mem.eql(u8, &old.key, &new.key)) return new.path;
    }
    return value;
}
