//! Serialized ARC-inserted LIR image for interpreter-shim execution.
//!
//! The parent process owns all semantic compilation. It publishes checked
//! artifacts, lowers through MIR/IR/LIR, inserts ARC, and serializes this image.
//! The child process only deserializes this image and initializes the LIR
//! interpreter; it never sees `ModuleEnv`, CIR, checked artifacts, MIR, or IR.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const layout_mod = @import("layout");
const mir = @import("mir");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const LowerIr = @import("lower_ir.zig");

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;

pub const MAGIC: u32 = 0x52494c52; // "RLIR" in little-endian bytes.
pub const FORMAT_VERSION: u32 = 1;

pub const RuntimeImageError = Allocator.Error || error{
    InvalidRuntimeImage,
    UnsupportedRuntimeImageVersion,
};

/// Direct interpreter entrypoint published by the parent.
pub const PlatformEntrypoint = extern struct {
    ordinal: u32,
    root_proc: LIR.LirProcSpecId,
};

/// Offset/length pair into the serialized image buffer.
pub const SliceRef = extern struct {
    offset: u64,
    len: u64,
};

/// Header at the start of every LIR runtime image.
pub const Header = extern struct {
    magic: u32,
    format_version: u32,
    total_size: u64,
    target_usize: u8,
    _padding: [7]u8 = [_]u8{0} ** 7,
    root_procs: SliceRef,
    platform_entrypoints: SliceRef,
    store: SerializedLirStore,
    layouts: SerializedLayoutStore,
};

/// Owned deserialized runtime program. This is the only semantic payload the
/// child interpreter needs.
pub const RuntimeProgram = struct {
    lowered: LowerIr.Result,
    platform_entrypoints: std.ArrayList(PlatformEntrypoint),
    target_usize: base.target.TargetUsize,

    pub fn deinit(self: *RuntimeProgram) void {
        self.platform_entrypoints.deinit(self.lowered.store.allocator);
        self.lowered.deinit();
    }
};

pub const SerializedLirStore = extern struct {
    cf_stmts: SliceRef,
    cf_switch_branches: SliceRef,
    locals: SliceRef,
    local_ids: SliceRef,
    proc_specs: SliceRef,
    strings: base.StringLiteral.Store.Serialized,
    next_synthetic_symbol: u64,

    pub fn serialize(
        self: *SerializedLirStore,
        store: *const LirStore,
        allocator: Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!void {
        self.cf_stmts = try appendSlice(writer, allocator, store.cf_stmts.items);
        self.cf_switch_branches = try appendSlice(writer, allocator, store.cf_switch_branches.items);
        self.locals = try appendSlice(writer, allocator, store.locals.items);
        self.local_ids = try appendSlice(writer, allocator, store.local_ids.items);
        self.proc_specs = try appendSlice(writer, allocator, store.proc_specs.items);
        try self.strings.serialize(&store.strings, allocator, writer);
        self.next_synthetic_symbol = store.next_synthetic_symbol;
    }

    pub fn deserializeOwned(
        self: *const SerializedLirStore,
        allocator: Allocator,
        base: usize,
    ) RuntimeImageError!LirStore {
        var borrowed_strings = self.strings.deserializeInto(base);
        const owned_strings = try borrowed_strings.clone(allocator);

        return .{
            .cf_stmts = try arrayListFromSerializedSlice(LIR.CFStmt, allocator, base, self.cf_stmts),
            .cf_switch_branches = try arrayListFromSerializedSlice(LIR.CFSwitchBranch, allocator, base, self.cf_switch_branches),
            .locals = try arrayListFromSerializedSlice(LIR.Local, allocator, base, self.locals),
            .local_ids = try arrayListFromSerializedSlice(LIR.LocalId, allocator, base, self.local_ids),
            .proc_specs = try arrayListFromSerializedSlice(LIR.LirProcSpec, allocator, base, self.proc_specs),
            .strings = owned_strings,
            .allocator = allocator,
            .next_synthetic_symbol = self.next_synthetic_symbol,
        };
    }
};

pub const SerializedLayoutStore = extern struct {
    layouts: collections.SafeList(layout_mod.Layout).Serialized,
    resolved_list_layouts: SliceRef,
    tuple_elems: collections.SafeList(layout_mod.Idx).Serialized,
    struct_fields: layout_mod.StructField.SafeMultiList.Serialized,
    struct_data: collections.SafeList(layout_mod.StructData).Serialized,
    tag_union_variants: layout_mod.TagUnionVariant.SafeMultiList.Serialized,
    tag_union_data: collections.SafeList(layout_mod.TagUnionData).Serialized,

    pub fn serialize(
        self: *SerializedLayoutStore,
        store: *const layout_mod.Store,
        allocator: Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!void {
        try self.layouts.serialize(&store.layouts, allocator, writer);
        self.resolved_list_layouts = try appendSlice(writer, allocator, store.resolved_list_layouts.items);
        try self.tuple_elems.serialize(&store.tuple_elems, allocator, writer);
        try self.struct_fields.serialize(&store.struct_fields, allocator, writer);
        try self.struct_data.serialize(&store.struct_data, allocator, writer);
        try self.tag_union_variants.serialize(&store.tag_union_variants, allocator, writer);
        try self.tag_union_data.serialize(&store.tag_union_data, allocator, writer);
    }

    pub fn deserializeOwned(
        self: *const SerializedLayoutStore,
        allocator: Allocator,
        base_addr: usize,
        target_usize: base.target.TargetUsize,
    ) RuntimeImageError!layout_mod.Store {
        return .{
            .all_module_envs = &.{},
            .allocator = allocator,
            .mutable_env = null,
            .layouts = try self.layouts.deserializeWithCopy(base_addr, allocator),
            .resolved_list_layouts = try arrayListFromSerializedSlice(?layout_mod.Idx, allocator, base_addr, self.resolved_list_layouts),
            .tuple_elems = try self.tuple_elems.deserializeWithCopy(base_addr, allocator),
            .struct_fields = try self.struct_fields.deserializeWithCopy(base_addr, allocator),
            .struct_data = try self.struct_data.deserializeWithCopy(base_addr, allocator),
            .tag_union_variants = try self.tag_union_variants.deserializeWithCopy(base_addr, allocator),
            .tag_union_data = try self.tag_union_data.deserializeWithCopy(base_addr, allocator),
            .interned_layouts = std.StringHashMap(layout_mod.Idx).init(allocator),
            .scratch_intern_key = .empty,
            .builtin_str_ident = null,
            .target_usize = target_usize,
        };
    }
};

pub fn serializeLoweredProgram(
    allocator: Allocator,
    lowered: *const LowerIr.Result,
    target_usize: base.target.TargetUsize,
    platform_entrypoints: []const PlatformEntrypoint,
) RuntimeImageError![]align(16) u8 {
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    const header = try writer.appendAlloc(allocator, Header);
    header.magic = MAGIC;
    header.format_version = FORMAT_VERSION;
    header.target_usize = @intFromEnum(target_usize);
    header.root_procs = try appendSlice(&writer, allocator, lowered.root_procs.items);
    header.platform_entrypoints = try appendSlice(&writer, allocator, platform_entrypoints);
    try header.store.serialize(&lowered.store, allocator, &writer);
    try header.layouts.serialize(&lowered.layouts, allocator, &writer);
    header.total_size = writer.total_bytes;

    const bytes = try allocator.alignedAlloc(u8, collections.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    errdefer allocator.free(bytes);
    _ = try writer.writeToBuffer(bytes);
    return bytes;
}

pub fn deserializeOwned(allocator: Allocator, bytes: []align(16) const u8) RuntimeImageError!RuntimeProgram {
    if (bytes.len < @sizeOf(Header)) return error.InvalidRuntimeImage;

    const base_addr = @intFromPtr(bytes.ptr);
    const header: *const Header = @ptrCast(@alignCast(bytes.ptr));
    if (header.magic != MAGIC) return error.InvalidRuntimeImage;
    if (header.format_version != FORMAT_VERSION) return error.UnsupportedRuntimeImageVersion;
    if (header.total_size != bytes.len) return error.InvalidRuntimeImage;

    var store = try header.store.deserializeOwned(allocator, base_addr);
    errdefer store.deinit();

    const target_usize: base.target.TargetUsize = switch (header.target_usize) {
        0 => .u32,
        1 => .u64,
        else => return error.InvalidRuntimeImage,
    };

    var layouts = try header.layouts.deserializeOwned(allocator, base_addr, target_usize);
    errdefer layouts.deinit();

    var root_procs = try arrayListFromSerializedSlice(LIR.LirProcSpecId, allocator, base_addr, header.root_procs);
    errdefer root_procs.deinit(allocator);

    var platform_entrypoints = try arrayListFromSerializedSlice(PlatformEntrypoint, allocator, base_addr, header.platform_entrypoints);
    errdefer platform_entrypoints.deinit(allocator);

    return .{
        .lowered = .{
            .canonical_names = mir.Hosted.CanonicalNameStore.init(allocator),
            .store = store,
            .layouts = layouts,
            .root_procs = root_procs,
            .proc_map = .empty,
        },
        .platform_entrypoints = platform_entrypoints,
        .target_usize = target_usize,
    };
}

fn appendSlice(
    writer: *CompactWriter,
    allocator: Allocator,
    slice: anytype,
) Allocator.Error!SliceRef {
    const relocated = try writer.appendSlice(allocator, slice);
    return .{
        .offset = @intFromPtr(relocated.ptr),
        .len = relocated.len,
    };
}

fn arrayListFromSerializedSlice(
    comptime T: type,
    allocator: Allocator,
    base: usize,
    slice_ref: SliceRef,
) RuntimeImageError!std.ArrayList(T) {
    if (slice_ref.len == 0) return .empty;

    const offset: usize = @intCast(slice_ref.offset);
    const len: usize = @intCast(slice_ref.len);
    const end = offset +% len * @sizeOf(T);
    if (end < offset) return error.InvalidRuntimeImage;

    const src_ptr: [*]const T = @ptrFromInt(base +% offset);
    const src = src_ptr[0..len];
    const owned = try allocator.alloc(T, len);
    @memcpy(owned, src);
    return std.ArrayList(T).fromOwnedSlice(owned);
}

test "runtime image declarations are referenced" {
    std.testing.refAllDecls(@This());
}
