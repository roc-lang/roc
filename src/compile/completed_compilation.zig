//! A completed compilation is an alternative to a live evaluation session.
//! Checked input identities are validated before consuming this outcome; no
//! checked module is marked evaluated by replaying it.
const std = @import("std");
const reporting = @import("reporting");
const Allocator = std.mem.Allocator;

pub const Module = struct {
    key: [32]u8,
    path: []const u8,
    identity_path: []const u8 = "",
};

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
pub const RootDemand = struct { root: Root, reads: []const Root };
pub const LiteralDemand = struct {
    module: [32]u8,
    checked_expr: u32,
    kind: @import("lir").LIR.LiteralRejectionKind,
    identity: ?[32]u8,
    reads: []const Root,
};
pub const Demand = struct {
    evaluated: []const RootDemand,
    stored: []const Root,
    literals: []const LiteralDemand,
};

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
            try write(info.tag_type.?, out, std.meta.activeTag(value));
            switch (value) {
                inline else => |v| try write(@TypeOf(v), out, v),
            }
        },
        else => @compileError("unsupported record field: " ++ @typeName(T)),
    }
}

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
            else => @compileError("unsupported record field: " ++ @typeName(T)),
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
        else => {},
    };
}

pub fn rebindPath(path: ?[]const u8, before: []const Module, after: []const Module) ?[]const u8 {
    const value = path orelse return null;
    for (before) |old| {
        if (!std.mem.eql(u8, old.path, value)) continue;
        for (after) |new| if (std.mem.eql(u8, &old.key, &new.key)) return new.path;
    }
    return value;
}
