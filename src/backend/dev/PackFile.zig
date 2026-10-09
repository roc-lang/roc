//! On-disk form of one module's pack: its procedure artifacts and the table
//! from specialization key to root artifact and ownership signature.
//!
//! The encoding is a pure function of its inputs (fixed-width little-endian
//! integers, length-prefixed bytes, artifacts in set order, specs in table
//! order), so two builds of the same module write identical files and the
//! store's rename-into-place protocol never has to compare contents.
//!
//! Line entries preserve exact SourceLoc values and order. Their file indices
//! are meaningful only with the originating LIR source-file domain; a persistent
//! consumer must bind that domain explicitly or omit lines from debug emission.

const std = @import("std");
const lir = @import("lir");
const ProcArtifact = @import("ProcArtifact.zig");
const RelocationMod = @import("Relocation.zig");
const CtfeContext = @import("CtfeContext.zig");
const ContextImmediate = @import("ContextImmediate.zig");

const Allocator = std.mem.Allocator;

const magic = "RPCK";
/// Format version; bump whenever the encoding or artifact contents change.
pub const format_version: u32 = 10;

/// One specialization the pack can serve: its reservation-time key, the
/// artifact holding its procedure, and the ownership signature and
/// uniqueness facts ARC solved for that procedure, which the linking program
/// adopts as fixed.
pub const SpecEntry = struct {
    key: [32]u8,
    artifact: u32,
    /// Null is an authoritative independent summary, not missing metadata.
    platform_requirement_relation: ?[32]u8 = null,
    rc_borrowed_params: u64,
    rc_ret_borrowed: bool,
    rc_ret_lenders: u64,
    rc_read_only_params: u64,
    rc_ret_unique: bool,
    rc_ret_unique_fields: u64,
    /// Each entry an `lir.LIR.RcRetCondition`.
    rc_ret_conditions: []const u32,
};

/// A pack read back from its bytes.
pub const Pack = struct {
    set: ProcArtifact.Set,
    specs: []const SpecEntry,

    pub fn deinit(self: *Pack) void {
        self.set.deinit();
    }
};

/// Why bytes could not be read as a pack.
pub const ReadError = Allocator.Error || error{
    MalformedPack,
    UnsupportedPackVersion,
};

pub const WriteError = Allocator.Error || ContextImmediate.Error;

/// Encode an artifact set and its spec table. Every carried constant the
/// set's program named for itself is written under its content name, and so
/// is every relocation to it, so the pack links into any program. Explicit
/// context immediates are stored as zero: their descriptors, not the producer's
/// dense IDs, decide the persistent bytes.
pub fn write(allocator: Allocator, set: *const ProcArtifact.Set, specs: []const SpecEntry) WriteError![]u8 {
    var names = try ProcArtifact.ContentNames.init(allocator, set);
    defer names.deinit();
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    var writer = Writer{ .allocator = allocator, .bytes = &bytes };

    try writer.raw(magic);
    try writer.word(format_version);
    try writer.word(@intCast(set.artifacts.len));
    try writer.word(@intCast(specs.len));

    for (set.artifacts) |artifact| {
        try writer.context(artifact.domain);
        try writer.context(artifact.context_contract);
        try writer.context(artifact.context_dependencies);
        try writer.context(artifact.context_complete);
        try writer.context(artifact.context_bindings);
        try writer.context(artifact.context_relocations);
        try writer.context(artifact.callable_contract);
        switch (artifact.kind) {
            .proc => |identity| {
                try writer.byte(0);
                try writer.raw(&identity.bytes);
            },
            .rc_helper => |name| {
                try writer.byte(1);
                try writer.str(name);
            },
            .boxy_thunk => |identity| {
                try writer.byte(2);
                try writer.raw(&identity.bytes);
            },
            .entrypoint => try writer.byte(3),
            .message_pool_run => try writer.byte(4),
            .branch_island => try writer.byte(5),
        }
        try writer.word(artifact.entry);
        if (artifact.frame) |frame| {
            try writer.byte(1);
            try writer.word(frame.prologue_size);
            try writer.word(frame.stack_alloc);
            try writer.word(frame.frame_size);
            try writer.word(frame.callee_saved_mask);
            try writer.word(frame.epilogue_offset);
            try writer.byte(@intFromBool(frame.uses_frame_pointer));
        } else {
            try writer.byte(0);
        }
        const code_offset = bytes.items.len + @sizeOf(u32);
        try writer.str(artifact.code);
        const stored_code = bytes.items[code_offset..][0..artifact.code.len];
        for (artifact.context_relocations) |relocation| {
            try ContextImmediate.patch(stored_code, relocation, 0);
        }
        try writer.word(@intCast(artifact.refs.len));
        for (artifact.refs) |ref| {
            try writer.word(ref.site);
            try writer.byte(switch (ref.form) {
                .call => 0,
                .addr => 1,
                .inline_call => 2,
            });
            try writer.word(ref.target);
            try writer.word(ref.delta);
            try writer.word(ref.veneer orelse std.math.maxInt(u32));
        }
        try writer.word(@intCast(artifact.symbolic_refs.len));
        for (artifact.symbolic_refs) |ref| {
            try writer.word(ref.site);
            try writer.byte(switch (ref.form) {
                .call => 0,
                .addr => 1,
                .inline_call => 2,
            });
            try writer.word(ref.veneer orelse std.math.maxInt(u32));
            switch (ref.target) {
                .proc => |identity| {
                    try writer.byte(0);
                    try writer.raw(&identity.bytes);
                },
                .rc_helper => |name| {
                    try writer.byte(1);
                    try writer.str(name);
                },
                .boxy_thunk => |identity| {
                    try writer.byte(2);
                    try writer.raw(&identity.bytes);
                },
            }
        }
        try writer.word(@intCast(artifact.lines.len));
        for (artifact.lines) |line| {
            try writer.word(line.offset);
            try writer.word(line.loc.file);
            try writer.word(line.loc.line);
            try writer.word(line.loc.column);
        }
        try writer.word(@intCast(artifact.relocations.len));
        for (artifact.relocations) |relocation| {
            try writer.word(relocation.offset);
            switch (relocation.kind) {
                .function => {
                    try writer.byte(0);
                    try writer.byte(0);
                },
                .data => |data_kind| {
                    try writer.byte(1);
                    try writer.byte(@intFromEnum(data_kind));
                },
            }
            try writer.str(names.of(relocation.name));
            // A reference to carried program data now names it by content.
            const scope: ProcArtifact.SymbolScope = if (names.renames(relocation.name)) .shared else relocation.scope;
            try writer.byte(@intFromEnum(scope));
        }
        try writer.word(@intCast(artifact.data.len));
        for (artifact.data) |item| {
            try writer.str(names.of(item.name));
            try writer.str(item.bytes);
            try writer.word(item.alignment);
            try writer.word(item.symbol_offset);
            try writer.word(@intCast(item.relocations.len));
            for (item.relocations) |relocation| {
                try writer.word(relocation.offset);
                try writer.wide(@bitCast(relocation.addend));
                try writer.byte(@intFromBool(relocation.function));
                try writer.byte(@intFromBool(relocation.external));
                // An external binding names the linking image's symbol.
                try writer.str(if (relocation.external) relocation.name else names.of(relocation.name));
            }
        }
    }

    for (specs) |spec| {
        try writer.raw(&spec.key);
        try writer.word(spec.artifact);
        try writer.byte(@intFromBool(spec.platform_requirement_relation != null));
        if (spec.platform_requirement_relation) |relation| try writer.raw(&relation);
        try writer.wide(spec.rc_borrowed_params);
        try writer.byte(@intFromBool(spec.rc_ret_borrowed));
        try writer.wide(spec.rc_ret_lenders);
        try writer.wide(spec.rc_read_only_params);
        try writer.byte(@intFromBool(spec.rc_ret_unique));
        try writer.wide(spec.rc_ret_unique_fields);
        try writer.word(@intCast(spec.rc_ret_conditions.len));
        for (spec.rc_ret_conditions) |condition| try writer.word(condition);
    }

    return try bytes.toOwnedSlice(allocator);
}

/// Decode a pack. Every slice in the result is owned by the pack's arena.
pub fn read(allocator: Allocator, bytes: []const u8) ReadError!Pack {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const arena_allocator = arena.allocator();
    var reader = Reader{ .bytes = bytes };

    const header = try reader.raw(magic.len);
    if (!std.mem.eql(u8, header, magic)) return error.MalformedPack;
    if (try reader.word() != format_version) return error.UnsupportedPackVersion;
    const artifact_count = try reader.word();
    const spec_count = try reader.word();

    const artifacts = try arena_allocator.alloc(ProcArtifact.Artifact, artifact_count);
    for (artifacts) |*artifact| {
        const domain = try reader.context(CtfeContext.Domain, arena_allocator);
        const context_contract = try reader.context(?@import("LirCodeGen.zig").FragmentContract, arena_allocator);
        const context_dependencies = try reader.context(?@import("LirCodeGen.zig").FragmentContextDependencies, arena_allocator);
        const context_complete = try reader.context(bool, arena_allocator);
        const context_bindings = try reader.context([]const CtfeContext.Binding, arena_allocator);
        const context_relocations = try reader.context([]const ContextImmediate.Relocation, arena_allocator);
        const callable_contract = try reader.context(?[32]u8, arena_allocator);
        if (domain == .runtime and (context_bindings.len != 0 or context_relocations.len != 0)) return error.MalformedPack;
        if (domain == .runtime and context_dependencies != null and context_dependencies.?.comptime_hooks) return error.MalformedPack;
        if (domain == .ctfe and (!context_complete or context_contract == null or context_dependencies == null or !context_contract.?.hooks_enabled or !context_dependencies.?.comptime_hooks)) return error.MalformedPack;
        const kind: ProcArtifact.Kind = switch (try reader.byte()) {
            0 => .{ .proc = .{ .bytes = (try reader.raw(32))[0..32].* } },
            1 => .{ .rc_helper = try reader.strOwned(arena_allocator) },
            2 => .{ .boxy_thunk = .{ .bytes = (try reader.raw(32))[0..32].* } },
            3 => .entrypoint,
            4 => .message_pool_run,
            5 => .branch_island,
            else => return error.MalformedPack,
        };
        const entry = try reader.word();
        const frame: ?ProcArtifact.Frame = switch (try reader.byte()) {
            0 => null,
            1 => .{
                .prologue_size = try reader.word(),
                .stack_alloc = try reader.word(),
                .frame_size = try reader.word(),
                .callee_saved_mask = try reader.word(),
                .epilogue_offset = try reader.word(),
                .uses_frame_pointer = switch (try reader.byte()) {
                    0 => false,
                    1 => true,
                    else => return error.MalformedPack,
                },
            },
            else => return error.MalformedPack,
        };
        const code = try reader.strOwned(arena_allocator);
        const refs = try arena_allocator.alloc(ProcArtifact.Reference, try reader.word());
        for (refs) |*ref| {
            ref.* = .{
                .site = try reader.word(),
                .form = switch (try reader.byte()) {
                    0 => .call,
                    1 => .addr,
                    2 => .inline_call,
                    else => return error.MalformedPack,
                },
                .target = try reader.word(),
                .delta = try reader.word(),
                .veneer = veneer: {
                    const value = try reader.word();
                    break :veneer if (value == std.math.maxInt(u32)) null else value;
                },
            };
        }
        const symbolic_refs = try arena_allocator.alloc(ProcArtifact.SymbolicReference, try reader.word());
        for (symbolic_refs) |*ref| {
            ref.* = .{
                .site = try reader.word(),
                .form = switch (try reader.byte()) {
                    0 => .call,
                    1 => .addr,
                    2 => .inline_call,
                    else => return error.MalformedPack,
                },
                .veneer = veneer: {
                    const value = try reader.word();
                    break :veneer if (value == std.math.maxInt(u32)) null else value;
                },
                .target = switch (try reader.byte()) {
                    0 => .{ .proc = .{ .bytes = (try reader.raw(32))[0..32].* } },
                    1 => .{ .rc_helper = try reader.strOwned(arena_allocator) },
                    2 => .{ .boxy_thunk = .{ .bytes = (try reader.raw(32))[0..32].* } },
                    else => return error.MalformedPack,
                },
            };
        }
        const lines = try arena_allocator.alloc(ProcArtifact.LineEntry, try reader.word());
        for (lines) |*line| {
            line.* = .{
                .offset = try reader.word(),
                .loc = .{ .file = try reader.word(), .line = try reader.word(), .column = try reader.word() },
            };
        }
        const relocations = try arena_allocator.alloc(ProcArtifact.NamedRelocation, try reader.word());
        for (relocations) |*relocation| {
            const offset = try reader.word();
            const kind_tag = try reader.byte();
            const data_kind_raw = try reader.byte();
            const name = try reader.strOwned(arena_allocator);
            const scope = std.enums.fromInt(ProcArtifact.SymbolScope, try reader.byte()) orelse return error.MalformedPack;
            relocation.* = .{
                .offset = offset,
                .name = name,
                .scope = scope,
                .kind = switch (kind_tag) {
                    0 => .function,
                    1 => .{ .data = std.enums.fromInt(RelocationMod.DataRelocationKind, data_kind_raw) orelse return error.MalformedPack },
                    else => return error.MalformedPack,
                },
            };
        }
        const data = try arena_allocator.alloc(ProcArtifact.DataItem, try reader.word());
        for (data) |*item| {
            const name = try reader.strOwned(arena_allocator);
            const item_bytes = try reader.strOwned(arena_allocator);
            const alignment = try reader.word();
            const symbol_offset = try reader.word();
            const data_relocations = try arena_allocator.alloc(ProcArtifact.DataRelocation, try reader.word());
            for (data_relocations) |*relocation| {
                const offset = try reader.word();
                const addend: i64 = @bitCast(try reader.wide());
                const function = switch (try reader.byte()) {
                    0 => false,
                    1 => true,
                    else => return error.MalformedPack,
                };
                const external = switch (try reader.byte()) {
                    0 => false,
                    1 => true,
                    else => return error.MalformedPack,
                };
                relocation.* = .{
                    .offset = offset,
                    .name = try reader.strOwned(arena_allocator),
                    .addend = addend,
                    .function = function,
                    .external = external,
                };
            }
            item.* = .{
                .name = name,
                .bytes = item_bytes,
                .alignment = alignment,
                .symbol_offset = symbol_offset,
                .relocations = data_relocations,
            };
        }
        artifact.* = .{
            .kind = kind,
            .code = code,
            .entry = entry,
            .frame = frame,
            .refs = refs,
            .symbolic_refs = symbolic_refs,
            .lines = lines,
            .relocations = relocations,
            .data = data,
            .domain = domain,
            .context_contract = context_contract,
            .context_dependencies = context_dependencies,
            .context_complete = context_complete,
            .context_bindings = context_bindings,
            .context_relocations = context_relocations,
            .callable_contract = callable_contract,
        };
        for (refs) |ref| {
            if (ref.target >= artifact_count) return error.MalformedPack;
        }
        ProcArtifact.validateContextPatches(artifact.*) catch return error.MalformedPack;
    }

    const specs = try arena_allocator.alloc(SpecEntry, spec_count);
    for (specs) |*spec| {
        spec.* = .{
            .key = (try reader.raw(32))[0..32].*,
            .artifact = try reader.word(),
            .platform_requirement_relation = switch (try reader.byte()) {
                0 => null,
                1 => (try reader.raw(32))[0..32].*,
                else => return error.MalformedPack,
            },
            .rc_borrowed_params = try reader.wide(),
            .rc_ret_borrowed = switch (try reader.byte()) {
                0 => false,
                1 => true,
                else => return error.MalformedPack,
            },
            .rc_ret_lenders = try reader.wide(),
            .rc_read_only_params = try reader.wide(),
            .rc_ret_unique = switch (try reader.byte()) {
                0 => false,
                1 => true,
                else => return error.MalformedPack,
            },
            .rc_ret_unique_fields = try reader.wide(),
            .rc_ret_conditions = &.{},
        };
        const conditions = try arena_allocator.alloc(u32, try reader.word());
        for (conditions) |*condition| condition.* = try reader.word();
        spec.rc_ret_conditions = conditions;
        if (spec.artifact >= artifact_count) return error.MalformedPack;
    }
    if (reader.offset != bytes.len) return error.MalformedPack;

    return .{
        .set = .{ .arena = arena, .artifacts = artifacts },
        .specs = specs,
    };
}

const Writer = struct {
    allocator: Allocator,
    bytes: *std.ArrayList(u8),

    // Typed metadata is serialized fieldwise, never including padding or host pointers.
    fn context(self: *Writer, value: anytype) Allocator.Error!void {
        const T = @TypeOf(value);
        switch (@typeInfo(T)) {
            .void => {},
            .bool => try self.byte(@intFromBool(value)),
            .int => |info| {
                if (info.bits == 8) try self.byte(value) else if (info.bits == 32) try self.word(value) else @compileError("unsupported context integer");
            },
            .@"enum" => try self.byte(@intCast(@intFromEnum(value))),
            .optional => {
                try self.byte(@intFromBool(value != null));
                if (value) |present| try self.context(present);
            },
            .array => for (value) |item| try self.context(item),
            .pointer => |info| {
                if (info.size != .slice) @compileError("context pointers must be slices");
                try self.word(@intCast(value.len));
                for (value) |item| try self.context(item);
            },
            .@"struct" => |info| inline for (info.fields) |field| try self.context(@field(value, field.name)),
            .@"union" => |info| {
                const tag = std.meta.activeTag(value);
                try self.context(tag);
                inline for (info.fields) |field| {
                    if (tag == @field(info.tag_type.?, field.name)) try self.context(@field(value, field.name));
                }
            },
            else => @compileError("unsupported context metadata"),
        }
    }

    fn raw(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.bytes.appendSlice(self.allocator, data);
    }

    fn byte(self: *Writer, value: u8) Allocator.Error!void {
        try self.bytes.append(self.allocator, value);
    }

    fn word(self: *Writer, value: u32) Allocator.Error!void {
        var buffer: [4]u8 = undefined;
        std.mem.writeInt(u32, &buffer, value, .little);
        try self.raw(&buffer);
    }

    fn wide(self: *Writer, value: u64) Allocator.Error!void {
        var buffer: [8]u8 = undefined;
        std.mem.writeInt(u64, &buffer, value, .little);
        try self.raw(&buffer);
    }

    fn str(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.word(@intCast(data.len));
        try self.raw(data);
    }
};

const Reader = struct {
    bytes: []const u8,
    offset: usize = 0,

    fn context(self: *Reader, comptime T: type, allocator: Allocator) ReadError!T {
        switch (@typeInfo(T)) {
            .void => return {},
            .bool => return switch (try self.byte()) {
                0 => false,
                1 => true,
                else => error.MalformedPack,
            },
            .int => |info| {
                if (info.bits == 8) return try self.byte();
                if (info.bits == 32) return try self.word();
                @compileError("unsupported context integer");
            },
            .@"enum" => return std.enums.fromInt(T, try self.byte()) orelse error.MalformedPack,
            .optional => |info| return switch (try self.byte()) {
                0 => null,
                1 => try self.context(info.child, allocator),
                else => error.MalformedPack,
            },
            .array => |info| {
                var result: T = undefined;
                for (&result) |*item| item.* = try self.context(info.child, allocator);
                return result;
            },
            .pointer => |info| {
                if (info.size != .slice) @compileError("context pointers must be slices");
                const len = try self.word();
                // Every supported element consumes at least one byte.
                if (len > self.bytes.len - self.offset) return error.MalformedPack;
                const items = try allocator.alloc(info.child, len);
                for (items) |*item| item.* = try self.context(info.child, allocator);
                return items;
            },
            .@"struct" => |info| {
                var result: T = undefined;
                inline for (info.fields) |field| @field(result, field.name) = try self.context(field.type, allocator);
                return result;
            },
            .@"union" => |info| {
                const tag = try self.context(info.tag_type.?, allocator);
                inline for (info.fields) |field| {
                    if (tag == @field(info.tag_type.?, field.name)) return @unionInit(T, field.name, try self.context(field.type, allocator));
                }
                return error.MalformedPack;
            },
            else => @compileError("unsupported context metadata"),
        }
    }

    fn raw(self: *Reader, len: usize) ReadError![]const u8 {
        if (self.bytes.len - self.offset < len) return error.MalformedPack;
        const slice = self.bytes[self.offset..][0..len];
        self.offset += len;
        return slice;
    }

    fn byte(self: *Reader) ReadError!u8 {
        return (try self.raw(1))[0];
    }

    fn word(self: *Reader) ReadError!u32 {
        return std.mem.readInt(u32, (try self.raw(4))[0..4], .little);
    }

    fn wide(self: *Reader) ReadError!u64 {
        return std.mem.readInt(u64, (try self.raw(8))[0..8], .little);
    }

    fn strOwned(self: *Reader, allocator: Allocator) ReadError![]u8 {
        const len = try self.word();
        return try allocator.dupe(u8, try self.raw(len));
    }
};

test "pack ownership survives input destruction and allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testOwnedPack, .{});
}

test "pack CTFE context clone serialization and corrupt binding rejection" {
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const source: CtfeContext.SourceDescriptor = .{
        .checked_module = [_]u8{1} ** 32,
        .source_identity = [_]u8{2} ** 32,
        .region = .{ .start = .{ .offset = 12 }, .end = .{ .offset = 24 } },
        .line = 3,
        .column = 7,
        .has_location = true,
    };
    const failure: CtfeContext.FailureDescriptor = .{
        .source = source,
        .checked_error = true,
        .literal_rejection = .{ .checked_module = [_]u8{3} ** 32, .checked_expr = 19, .kind = .numeral },
        .guard_root = null,
    };
    const code = [_]u8{ 0x48, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0 } ++ [_]u8{0} ** 32;
    var artifacts = [_]ProcArtifact.Artifact{.{
        .kind = .entrypoint,
        .code = &code,
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .domain = .ctfe,
        .context_dependencies = .{ .comptime_hooks = true },
        .context_contract = .{
            .target = .x64glibc,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
            .dict_seed_mode = .comptime_zero,
            .hooks_enabled = true,
            .initialize_boxy_runtime = false,
            .static_data_readonly = false,
        },
        .context_bindings = &.{.{ .failure = failure }},
        .context_relocations = &.{.{ .offset = 0, .binding = 0, .encoding = .x86_movabs }},
    }};
    var input = ProcArtifact.Set{ .arena = arena, .artifacts = &artifacts };
    var cloned = try ProcArtifact.combine(allocator, &.{&input});
    defer cloned.deinit();
    const bytes = try write(allocator, &cloned, &.{});
    defer allocator.free(bytes);
    var decoded = try read(allocator, bytes);
    defer decoded.set.deinit();
    try std.testing.expectEqualDeep(cloned.artifacts[0], decoded.set.artifacts[0]);
    const Resolver = struct {
        fn resolve(context: *anyopaque, binding: CtfeContext.Binding) ?u64 {
            const id: *u64 = @ptrCast(@alignCast(context));
            return switch (binding) {
                .failure => id.*,
                else => null,
            };
        }
    };
    var first_id: u64 = 41;
    var second_id: u64 = 97;
    var first = try ProcArtifact.bindContextClone(allocator, &decoded.set, .{ .context = &first_id, .resolve = Resolver.resolve });
    defer first.deinit();
    var second = try ProcArtifact.bindContextClone(allocator, &decoded.set, .{ .context = &second_id, .resolve = Resolver.resolve });
    defer second.deinit();
    try std.testing.expectEqual(@as(u64, 41), std.mem.readInt(u64, first.artifacts[0].code[2..10], .little));
    try std.testing.expectEqual(@as(u64, 97), std.mem.readInt(u64, second.artifacts[0].code[2..10], .little));
    try std.testing.expectEqual(@as(u64, 0), std.mem.readInt(u64, decoded.set.artifacts[0].code[2..10], .little));
    const first_bytes = try write(allocator, &first, &.{});
    defer allocator.free(first_bytes);
    const second_bytes = try write(allocator, &second, &.{});
    defer allocator.free(second_bytes);
    try std.testing.expectEqualSlices(u8, bytes, first_bytes);
    try std.testing.expectEqualSlices(u8, bytes, second_bytes);
    try std.testing.expectEqual(@as(u64, 41), std.mem.readInt(u64, first.artifacts[0].code[2..10], .little));
    try std.testing.expectEqual(@as(u64, 97), std.mem.readInt(u64, second.artifacts[0].code[2..10], .little));
    artifacts[0].domain = .runtime;
    const wrong_domain = try write(allocator, &input, &.{});
    defer allocator.free(wrong_domain);
    try std.testing.expectError(error.MalformedPack, read(allocator, wrong_domain));
    artifacts[0].domain = .ctfe;
    artifacts[0].context_relocations = &.{.{ .offset = 0, .binding = 1, .encoding = .x86_movabs }};
    const missing = try write(allocator, &input, &.{});
    defer allocator.free(missing);
    try std.testing.expectError(error.MalformedPack, read(allocator, missing));
    artifacts[0].context_relocations = &.{};
    const removed = try write(allocator, &input, &.{});
    defer allocator.free(removed);
    try std.testing.expectError(error.MalformedPack, read(allocator, removed));
    try std.testing.expectError(error.MissingContextBinding, ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve }));
    try std.testing.expectEqualSlices(u8, &code, input.artifacts[0].code);
    artifacts[0].context_relocations = &.{.{ .offset = 0, .binding = 0, .encoding = .x86_movabs }};

    artifacts[0].relocations = &.{.{ .offset = 2, .name = "ordinary", .scope = .shared, .kind = .{ .data = .abs64 } }};
    const overlap = try write(allocator, &input, &.{});
    defer allocator.free(overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, overlap));
    try std.testing.expectError(error.InvalidContextRelocation, ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve }));
    try std.testing.expectEqualSlices(u8, &code, input.artifacts[0].code);
    artifacts[0].relocations = &.{.{ .offset = 6, .name = "ordinary", .scope = .shared, .kind = .function }};
    const function_overlap = try write(allocator, &input, &.{});
    defer allocator.free(function_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, function_overlap));
    try std.testing.expectError(error.InvalidContextRelocation, ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve }));
    artifacts[0].relocations = &.{};
    artifacts[0].refs = &.{.{ .site = 0, .form = .addr, .target = 0, .delta = 0 }};
    const ref_overlap = try write(allocator, &input, &.{});
    defer allocator.free(ref_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, ref_overlap));
    try std.testing.expectError(error.InvalidContextRelocation, ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve }));
    artifacts[0].refs = &.{};
    artifacts[0].symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .rc_helper = "ordinary" } }};
    const symbolic_overlap = try write(allocator, &input, &.{});
    defer allocator.free(symbolic_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, symbolic_overlap));
    artifacts[0].symbolic_refs = &.{};
    // Adjacent ordinary code patches and same-offset data-section patches
    // have distinct owners and must remain valid.
    artifacts[0].relocations = &.{.{ .offset = 10, .name = "ordinary", .scope = .shared, .kind = .{ .data = .abs64 } }};
    artifacts[0].refs = &.{.{ .site = 18, .form = .call, .target = 0, .delta = 0 }};
    artifacts[0].data = &.{.{
        .name = "datum",
        .bytes = &([_]u8{0} ** 16),
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &.{.{ .offset = 2, .name = "ordinary", .addend = 0, .function = false }},
    }};
    const adjacent = try write(allocator, &input, &.{});
    defer allocator.free(adjacent);
    var accepted = try read(allocator, adjacent);
    defer accepted.set.deinit();
    var bound_adjacent = try ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve });
    defer bound_adjacent.deinit();

    var arm_code = [_]u8{0} ** 64;
    for (0..4) |i| {
        const word: u32 = (if (i == 0) @as(u32, 0xd2800000) else 0xf2800000) |
            (@as(u32, @intCast(i)) << 21) | 7;
        std.mem.writeInt(u32, arm_code[16 + i * 4 ..][0..4], word, .little);
    }
    artifacts[0].code = &arm_code;
    artifacts[0].context_contract.?.target = .arm64glibc;
    artifacts[0].context_relocations = &.{.{ .offset = 16, .binding = 0, .encoding = .arm_movwide }};
    artifacts[0].refs = &.{};
    for ([_]RelocationMod.DataRelocationKind{ .abs64, .rel32, .page21, .pageoff12 }) |kind| {
        const ordinary = [_]ProcArtifact.NamedRelocation{.{ .offset = 28, .name = "ordinary", .scope = .shared, .kind = .{ .data = kind } }};
        artifacts[0].relocations = &ordinary;
        const arm_overlap = try write(allocator, &input, &.{});
        defer allocator.free(arm_overlap);
        try std.testing.expectError(error.MalformedPack, read(allocator, arm_overlap));
        try std.testing.expectError(error.InvalidContextRelocation, ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve }));
    }
    artifacts[0].relocations = &.{};
    for ([_]ProcArtifact.Form{ .addr, .inline_call }) |form| {
        const references = [_]ProcArtifact.Reference{.{ .site = 4, .form = form, .target = 0, .delta = 0 }};
        artifacts[0].refs = &references;
        const arm_ref_overlap = try write(allocator, &input, &.{});
        defer allocator.free(arm_ref_overlap);
        try std.testing.expectError(error.MalformedPack, read(allocator, arm_ref_overlap));
        try std.testing.expectError(error.InvalidContextRelocation, ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve }));
    }
    artifacts[0].refs = &.{.{ .site = 0, .form = .call, .target = 0, .delta = 0, .veneer = 4 }};
    const veneer_overlap = try write(allocator, &input, &.{});
    defer allocator.free(veneer_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, veneer_overlap));
    artifacts[0].refs = &.{
        .{ .site = 0, .form = .addr, .target = 0, .delta = 0 },
        .{ .site = 32, .form = .inline_call, .target = 0, .delta = 0 },
    };
    artifacts[0].relocations = &.{
        .{ .offset = 48, .name = "ordinary", .scope = .shared, .kind = .{ .data = .page21 } },
        .{ .offset = 52, .name = "ordinary", .scope = .shared, .kind = .{ .data = .pageoff12 } },
    };
    const arm_adjacent = try write(allocator, &input, &.{});
    defer allocator.free(arm_adjacent);
    var arm_accepted = try read(allocator, arm_adjacent);
    defer arm_accepted.set.deinit();
    first_id = 0;
    var arm_zero = try ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve });
    defer arm_zero.deinit();
    for (0..4) |i| {
        const word = std.mem.readInt(u32, arm_zero.artifacts[0].code[16 + i * 4 ..][0..4], .little);
        try std.testing.expectEqual(@as(u32, 0), (word >> 5) & 0xffff);
    }
    try std.testing.expectEqualSlices(u8, &arm_code, input.artifacts[0].code);
    first_id = 73;
    var arm_displaced = try ProcArtifact.bindContextClone(allocator, &input, .{ .context = &first_id, .resolve = Resolver.resolve });
    defer arm_displaced.deinit();
    const arm_zero_bytes = try write(allocator, &arm_zero, &.{});
    defer allocator.free(arm_zero_bytes);
    const arm_displaced_bytes = try write(allocator, &arm_displaced, &.{});
    defer allocator.free(arm_displaced_bytes);
    try std.testing.expectEqualSlices(u8, arm_zero_bytes, arm_displaced_bytes);
    try std.testing.expectEqual(@as(u32, 73), (std.mem.readInt(u32, arm_displaced.artifacts[0].code[16..20], .little) >> 5) & 0xffff);
}

fn testOwnedPack(allocator: Allocator) (ReadError || WriteError || error{TestExpectedEqual})!void {
    const set = ProcArtifact.Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{.{
            .kind = .{ .rc_helper = "helper" },
            .callable_contract = [_]u8{17} ** 32,
            .code = "code",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .call, .target = 0, .delta = 0, .veneer = 3 }},
            .symbolic_refs = &.{.{ .site = 1, .form = .call, .target = .{ .rc_helper = "external" } }},
            .lines = &.{.{ .offset = 0, .loc = .{ .file = 2, .line = 10, .column = 8 } }},
            .relocations = &.{.{ .offset = 0, .name = "builtin", .scope = .shared, .kind = .function }},
            .data = &.{.{
                .name = "datum",
                .bytes = "bytes",
                .alignment = 8,
                .symbol_offset = 1,
                .relocations = &.{.{ .offset = 0, .name = "target", .addend = -2, .function = true, .external = true }},
            }},
        }},
    };
    const bytes = try write(allocator, &set, &.{});
    var bytes_live = true;
    defer if (bytes_live) allocator.free(bytes);
    var pack = try read(allocator, bytes);
    defer pack.deinit();
    allocator.free(bytes);
    bytes_live = false;
    try std.testing.expectEqualDeep(set.artifacts, pack.set.artifacts);
}

test "pack bytes round-trip every artifact field and spec entry" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const a = arena.allocator();
    const refs = try a.dupe(ProcArtifact.Reference, &.{
        .{ .site = 5, .form = .call, .target = 1, .delta = 8 },
        .{ .site = 12, .form = .addr, .target = 0, .delta = 0 },
    });
    const relocations = try a.dupe(ProcArtifact.NamedRelocation, &.{
        .{ .offset = 3, .name = try a.dupe(u8, "roc_builtins_str_concat"), .scope = .shared, .kind = .function },
        .{ .offset = 9, .name = try a.dupe(u8, "roc__d1"), .scope = .program, .kind = .{ .data = .rel32 } },
    });
    const artifacts = try a.dupe(ProcArtifact.Artifact, &.{
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(7) },
            .code = try a.dupe(u8, &.{ 0x55, 0x48, 0x89, 0xe5, 0xc3 }),
            .entry = 0,
            .frame = .{ .prologue_size = 4, .stack_alloc = 16, .frame_size = 16, .callee_saved_mask = 0x1000, .epilogue_offset = 4, .uses_frame_pointer = true },
            .refs = refs,
            .symbolic_refs = &.{
                .{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(8) } },
                .{ .site = 1, .form = .call, .veneer = 4, .target = .{ .rc_helper = "roc__rc_later" } },
                .{ .site = 2, .form = .addr, .target = .{ .boxy_thunk = lir.ProcIdentity.forTest(9) } },
            },
            .lines = &.{
                .{ .offset = 0, .loc = .{ .file = 3, .line = 42, .column = 7 } },
                .{ .offset = 0, .loc = .{ .file = 3, .line = 43, .column = 9 } },
            },
            .relocations = relocations,
            .data = try a.dupe(ProcArtifact.DataItem, &.{
                .{ .name = try a.dupe(u8, "roc__hab"), .bytes = try a.dupe(u8, "\x00\x00hi"), .alignment = 8, .symbol_offset = 2 },
                .{
                    .name = try a.dupe(u8, "roc__hcd"),
                    .bytes = try a.dupe(u8, "\x00" ** 16),
                    .alignment = 8,
                    .symbol_offset = 0,
                    .relocations = try a.dupe(ProcArtifact.DataRelocation, &.{
                        .{ .offset = 0, .name = try a.dupe(u8, "roc__hab"), .addend = 2, .function = false },
                        .{ .offset = 8, .name = try a.dupe(u8, "roc__rc_decref_abc"), .addend = -1, .function = true },
                    }),
                },
            }),
        },
        .{
            .kind = .{ .rc_helper = try a.dupe(u8, "roc__rc_decref_abc") },
            .code = try a.dupe(u8, &.{ 0xeb, 0x02, 0x90, 0xc3 }),
            .entry = 2,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        },
        .{ .kind = .message_pool_run, .code = try a.dupe(u8, "hello"), .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    });
    var set = ProcArtifact.Set{ .arena = arena, .artifacts = artifacts };
    defer set.deinit();
    const specs = [_]SpecEntry{
        .{
            .key = [_]u8{0xab} ** 32,
            .artifact = 0,
            .rc_borrowed_params = 0b101,
            .rc_ret_borrowed = true,
            .rc_ret_lenders = 1,
            .rc_read_only_params = 0b100,
            .rc_ret_unique = true,
            .rc_ret_unique_fields = 0b10,
            .rc_ret_conditions = &.{ 0x0001_02ff, 0x0000_0403 },
        },
    };

    const bytes = try write(testing.allocator, &set, &specs);
    defer testing.allocator.free(bytes);
    const again = try write(testing.allocator, &set, &specs);
    defer testing.allocator.free(again);
    try testing.expectEqualSlices(u8, bytes, again);

    var pack = try read(testing.allocator, bytes);
    defer pack.deinit();
    try testing.expectEqual(@as(usize, 3), pack.set.artifacts.len);
    try testing.expectEqual(@as(usize, 1), pack.specs.len);
    try testing.expectEqualSlices(u8, &specs[0].key, &pack.specs[0].key);
    try testing.expectEqual(specs[0].rc_borrowed_params, pack.specs[0].rc_borrowed_params);
    try testing.expect(pack.specs[0].rc_ret_borrowed);
    try testing.expectEqual(specs[0].rc_read_only_params, pack.specs[0].rc_read_only_params);
    try testing.expect(pack.specs[0].rc_ret_unique);
    try testing.expectEqual(specs[0].rc_ret_unique_fields, pack.specs[0].rc_ret_unique_fields);
    try testing.expectEqualSlices(u32, specs[0].rc_ret_conditions, pack.specs[0].rc_ret_conditions);
    const proc = pack.set.artifacts[0];
    try testing.expectEqualSlices(u8, &lir.ProcIdentity.forTest(7).bytes, &proc.kind.proc.bytes);
    try testing.expectEqualSlices(u8, artifacts[0].code, proc.code);
    try testing.expectEqual(@as(usize, 2), proc.refs.len);
    try testing.expectEqual(@as(usize, 3), proc.symbolic_refs.len);
    try testing.expectEqualDeep(artifacts[0].symbolic_refs, proc.symbolic_refs);
    try testing.expectEqualDeep(artifacts[0].lines, proc.lines);
    try testing.expectEqual(ProcArtifact.Form.addr, proc.refs[1].form);
    try testing.expectEqual(@as(u32, 9), proc.relocations[1].offset);
    try testing.expectEqualStrings("roc__d1", proc.relocations[1].name);
    try testing.expectEqual(ProcArtifact.SymbolScope.program, proc.relocations[1].scope);
    try testing.expectEqual(ProcArtifact.SymbolScope.shared, proc.relocations[0].scope);
    try testing.expectEqual(RelocationMod.DataRelocationKind.rel32, proc.relocations[1].kind.data);
    try testing.expectEqual(@as(u32, 0x1000), proc.frame.?.callee_saved_mask);
    try testing.expectEqual(@as(usize, 2), proc.data.len);
    try testing.expectEqualStrings("roc__hab", proc.data[0].name);
    try testing.expectEqual(@as(u32, 2), proc.data[0].symbol_offset);
    try testing.expectEqual(@as(usize, 0), proc.data[0].relocations.len);
    try testing.expectEqualStrings("roc__hcd", proc.data[1].name);
    try testing.expectEqual(@as(usize, 2), proc.data[1].relocations.len);
    try testing.expectEqual(@as(u32, 8), proc.data[1].relocations[1].offset);
    try testing.expectEqual(@as(i64, -1), proc.data[1].relocations[1].addend);
    try testing.expect(proc.data[1].relocations[1].function);
    try testing.expectEqualStrings("roc__rc_decref_abc", proc.data[1].relocations[1].name);
    try testing.expect(!proc.data[1].relocations[0].function);
    try testing.expectEqual(@as(i64, 2), proc.data[1].relocations[0].addend);
    try testing.expectEqualStrings("roc__rc_decref_abc", pack.set.artifacts[1].kind.rc_helper);
    try testing.expectEqual(@as(u32, 2), pack.set.artifacts[1].entry);
    try testing.expect(pack.set.artifacts[1].frame == null);

    const rewritten = try write(testing.allocator, &pack.set, pack.specs);
    defer testing.allocator.free(rewritten);
    try testing.expectEqualSlices(u8, bytes, rewritten);

    try testing.expectError(error.MalformedPack, read(testing.allocator, bytes[0 .. bytes.len - 1]));
}

test "pack preserves independent and relation-dependent semantic summaries" {
    const Attempt = struct {
        fn run(allocator: Allocator, relation: ?[32]u8) (ReadError || WriteError || error{ TestExpectedEqual, TestExpectedError, TestUnexpectedError })!void {
            const set = ProcArtifact.Set{
                .arena = std.heap.ArenaAllocator.init(allocator),
                .artifacts = &.{.{
                    .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
                    .code = "code",
                    .entry = 0,
                    .frame = null,
                    .refs = &.{},
                    .relocations = &.{},
                    .data = &.{},
                }},
            };
            const bytes = try write(allocator, &set, &.{.{
                .key = [_]u8{7} ** 32,
                .artifact = 0,
                .platform_requirement_relation = relation,
                .rc_borrowed_params = 1,
                .rc_ret_borrowed = true,
                .rc_ret_lenders = 1,
                .rc_read_only_params = 1,
                .rc_ret_unique = true,
                .rc_ret_unique_fields = 2,
                .rc_ret_conditions = &.{0x0001_02ff},
            }});
            defer allocator.free(bytes);
            var pack = try read(allocator, bytes);
            defer pack.deinit();
            try std.testing.expectEqualDeep(relation, pack.specs[0].platform_requirement_relation);
            const rewritten = try write(allocator, &pack.set, pack.specs);
            defer allocator.free(rewritten);
            try std.testing.expectEqualSlices(u8, bytes, rewritten);
            // A previous contract must decline before admitting any summary.
            const old = try allocator.dupe(u8, bytes);
            defer allocator.free(old);
            std.mem.writeInt(u32, old[4..8], format_version - 1, .little);
            try std.testing.expectError(error.UnsupportedPackVersion, read(allocator, old));
        }
    };
    for ([_]?[32]u8{ null, [_]u8{0} ** 32, [_]u8{9} ** 32 }) |relation| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, Attempt.run, .{relation});
    }
}

test "pack writes program-local constants and every reference to them under content names" {
    const testing = std.testing;
    // Two programs gave one program-local name to different constants.
    const names = [_][]const u8{ "first", "second" };
    var packed_names: [names.len][]const u8 = undefined;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    for (names, 0..) |bytes, index| {
        const set = ProcArtifact.Set{
            .arena = std.heap.ArenaAllocator.init(testing.allocator),
            .artifacts = &.{.{
                .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
                .code = "code",
                .entry = 0,
                .frame = null,
                .refs = &.{},
                .relocations = &.{.{ .offset = 0, .name = "roc__d1", .scope = .program, .kind = .{ .data = .rel32 } }},
                .data = &.{
                    .{
                        .name = "roc__d1",
                        .bytes = "\x00" ** 8,
                        .alignment = 8,
                        .symbol_offset = 0,
                        .relocations = &.{.{ .offset = 0, .name = "roc__d1_1", .addend = 16, .function = false }},
                        .program_local_name = true,
                    },
                    .{
                        .name = "roc__d1_1",
                        .bytes = bytes,
                        .alignment = 8,
                        .symbol_offset = 0,
                        .relocations = &.{.{ .offset = 0, .name = "roc__d1_1", .addend = 0, .function = false, .external = true }},
                        .program_local_name = true,
                    },
                },
            }},
        };
        const encoded = try write(testing.allocator, &set, &.{});
        defer testing.allocator.free(encoded);
        var pack = try read(testing.allocator, encoded);
        defer pack.deinit();
        const artifact = pack.set.artifacts[0];
        const root = artifact.data[0];
        const node = artifact.data[1];
        try testing.expect(std.mem.startsWith(u8, root.name, ProcArtifact.content_data_prefix));
        try testing.expect(std.mem.startsWith(u8, node.name, ProcArtifact.content_data_prefix));
        try testing.expectEqualStrings(root.name, artifact.relocations[0].name);
        // Named by content, the reference means the same in every program.
        try testing.expectEqual(ProcArtifact.SymbolScope.shared, artifact.relocations[0].scope);
        try testing.expectEqualStrings(node.name, root.relocations[0].name);
        // An external binding keeps the linking image's name, even one the
        // set also carries a datum under.
        try testing.expectEqualStrings("roc__d1_1", node.relocations[0].name);
        packed_names[index] = try arena.allocator().dupe(u8, node.name);
    }
    try testing.expect(!std.mem.eql(u8, packed_names[0], packed_names[1]));
}
