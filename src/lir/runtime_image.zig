//! Shared-memory ARC-inserted LIR image for interpreter-shim execution.
//!
//! The parent process owns all semantic compilation. It publishes checked
//! artifacts, lowers through MIR/IR/LIR, inserts ARC, and then publishes a small
//! offset table into the existing shared-memory allocator. The child process maps
//! the same shared-memory object and views the LIR/runtime-layout arrays in
//! place; it never reconstructs compiler data.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const LowerIr = @import("lower_ir.zig");

/// Public `MAGIC` declaration.
pub const MAGIC: u32 = 0x52494c52; // "RLIR" in little-endian bytes.
/// Public `FORMAT_VERSION` declaration.
pub const FORMAT_VERSION: u32 = 1;

/// Public `ImageError` declaration.
pub const ImageError = error{
    InvalidRuntimeImage,
    UnsupportedRuntimeImageVersion,
};

/// Direct interpreter entrypoint published by the parent.
pub const PlatformEntrypoint = extern struct {
    ordinal: u32,
    root_proc: LIR.LirProcSpecId,
};

/// Offset/length/capacity of one array inside the shared-memory mapping.
pub const ArrayRef = extern struct {
    offset: u64,
    len: u64,
    capacity: u64,

    pub fn empty() ArrayRef {
        return .{ .offset = 0, .len = 0, .capacity = 0 };
    }
};

/// Header stored as the first user allocation after `SharedMemoryAllocator.Header`.
pub const Header = extern struct {
    magic: u32,
    format_version: u32,
    image_size: u64,
    target_usize: u8,
    _padding: [7]u8 = [_]u8{0} ** 7,
    root_procs: ArrayRef,
    platform_entrypoints: ArrayRef,
    store: LirStoreImage,
    layouts: LayoutStoreImage,
};

/// A child-side view over mapped shared memory. This value owns no compiler
/// storage. Do not call `deinit` on `store` or `layouts`; unmapping the shared
/// memory releases the storage.
pub const ProgramView = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: []LIR.LirProcSpecId,
    platform_entrypoints: []PlatformEntrypoint,
    target_usize: base.target.TargetUsize,
};

/// Public `LirStoreImage` declaration.
pub const LirStoreImage = extern struct {
    cf_stmts: ArrayRef,
    cf_switch_branches: ArrayRef,
    locals: ArrayRef,
    local_ids: ArrayRef,
    proc_specs: ArrayRef,
    strings: StringLiteralStoreImage,
    next_synthetic_symbol: u64,

    fn fromStore(base_ptr: [*]align(1) const u8, image_size: usize, store: *const LirStore) ImageError!LirStoreImage {
        return .{
            .cf_stmts = try arrayRef(base_ptr, image_size, store.cf_stmts.items),
            .cf_switch_branches = try arrayRef(base_ptr, image_size, store.cf_switch_branches.items),
            .locals = try arrayRef(base_ptr, image_size, store.locals.items),
            .local_ids = try arrayRef(base_ptr, image_size, store.local_ids.items),
            .proc_specs = try arrayRef(base_ptr, image_size, store.proc_specs.items),
            .strings = try StringLiteralStoreImage.fromStore(base_ptr, image_size, &store.strings),
            .next_synthetic_symbol = store.next_synthetic_symbol,
        };
    }

    fn view(self: LirStoreImage, base_ptr: [*]align(1) u8, image_size: usize) ImageError!LirStore {
        return .{
            .cf_stmts = arrayListFromRef(LIR.CFStmt, base_ptr, image_size, self.cf_stmts),
            .cf_switch_branches = arrayListFromRef(LIR.CFSwitchBranch, base_ptr, image_size, self.cf_switch_branches),
            .locals = arrayListFromRef(LIR.Local, base_ptr, image_size, self.locals),
            .local_ids = arrayListFromRef(LIR.LocalId, base_ptr, image_size, self.local_ids),
            .proc_specs = arrayListFromRef(LIR.LirProcSpec, base_ptr, image_size, self.proc_specs),
            .strings = try self.strings.view(base_ptr, image_size),
            .allocator = std.heap.page_allocator,
            .next_synthetic_symbol = self.next_synthetic_symbol,
        };
    }
};

/// Public `StringLiteralStoreImage` declaration.
pub const StringLiteralStoreImage = extern struct {
    buffer: ArrayRef,

    fn fromStore(base_ptr: [*]align(1) const u8, image_size: usize, store: *const base.StringLiteral.Store) ImageError!StringLiteralStoreImage {
        return .{
            .buffer = try arrayRef(base_ptr, image_size, store.buffer.items.items),
        };
    }

    fn view(self: StringLiteralStoreImage, base_ptr: [*]align(1) u8, image_size: usize) ImageError!base.StringLiteral.Store {
        return .{
            .buffer = stringLiteralBufferFromRef(base_ptr, image_size, self.buffer),
        };
    }
};

/// Public `LayoutStoreImage` declaration.
pub const LayoutStoreImage = extern struct {
    layouts: ArrayRef,
    resolved_list_layouts: ArrayRef,
    tuple_elems: ArrayRef,
    struct_fields: ArrayRef,
    struct_data: ArrayRef,
    tag_union_variants: ArrayRef,
    tag_union_data: ArrayRef,

    fn fromStore(base_ptr: [*]align(1) const u8, image_size: usize, store: *const layout_mod.Store) ImageError!LayoutStoreImage {
        return .{
            .layouts = try arrayRef(base_ptr, image_size, store.layouts.items.items),
            .resolved_list_layouts = try arrayRef(base_ptr, image_size, store.resolved_list_layouts.items),
            .tuple_elems = try arrayRef(base_ptr, image_size, store.tuple_elems.items.items),
            .struct_fields = try multiArrayRef(layout_mod.StructField, base_ptr, image_size, store.struct_fields),
            .struct_data = try arrayRef(base_ptr, image_size, store.struct_data.items.items),
            .tag_union_variants = try multiArrayRef(layout_mod.TagUnionVariant, base_ptr, image_size, store.tag_union_variants),
            .tag_union_data = try arrayRef(base_ptr, image_size, store.tag_union_data.items.items),
        };
    }

    fn view(
        self: LayoutStoreImage,
        base_ptr: [*]align(1) u8,
        image_size: usize,
        target_usize: base.target.TargetUsize,
    ) ImageError!layout_mod.Store {
        return .{
            .allocator = std.heap.page_allocator,
            .layouts = safeListFromRef(layout_mod.Layout, base_ptr, image_size, self.layouts),
            .resolved_list_layouts = arrayListFromRef(?layout_mod.Idx, base_ptr, image_size, self.resolved_list_layouts),
            .tuple_elems = safeListFromRef(layout_mod.Idx, base_ptr, image_size, self.tuple_elems),
            .struct_fields = safeMultiListFromRef(layout_mod.StructField, base_ptr, image_size, self.struct_fields),
            .struct_data = safeListFromRef(layout_mod.StructData, base_ptr, image_size, self.struct_data),
            .tag_union_variants = safeMultiListFromRef(layout_mod.TagUnionVariant, base_ptr, image_size, self.tag_union_variants),
            .tag_union_data = safeListFromRef(layout_mod.TagUnionData, base_ptr, image_size, self.tag_union_data),
            .interned_layouts = std.StringHashMap(layout_mod.Idx).init(std.heap.page_allocator),
            .scratch_intern_key = .empty,
            .target_usize = target_usize,
        };
    }
};

/// Fill the reserved runtime-image header in a contiguous buffer.
///
/// `lowered` must already have been allocated from an allocator that owns
/// the buffer at `base_ptr` (the buffer must contain every pointer reachable
/// from `lowered`). This function only installs offset metadata — it does not
/// copy data.
///
/// This is the IPC-agnostic variant. Use it for in-process embedders that
/// place the runtime image in a plain arena instead of shared memory.
pub fn fillHeaderInBuffer(
    header: *Header,
    base_ptr: [*]align(1) const u8,
    image_size: usize,
    lowered: *const LowerIr.Result,
    target_usize: base.target.TargetUsize,
    platform_entrypoints: []const PlatformEntrypoint,
) ImageError!void {
    header.* = .{
        .magic = MAGIC,
        .format_version = FORMAT_VERSION,
        .image_size = image_size,
        .target_usize = @intFromEnum(target_usize),
        .root_procs = try arrayRef(base_ptr, image_size, lowered.root_procs.items),
        .platform_entrypoints = try arrayRef(base_ptr, image_size, platform_entrypoints),
        .store = try LirStoreImage.fromStore(base_ptr, image_size, &lowered.store),
        .layouts = try LayoutStoreImage.fromStore(base_ptr, image_size, &lowered.layouts),
    };
}

/// Fill the reserved runtime-image header in the existing shared-memory mapping.
///
/// `lowered` must already have been allocated with the shared-memory allocator
/// associated with `base_ptr`; this function only installs offset metadata.
///
/// Thin wrapper over `fillHeaderInBuffer` — kept for naming clarity at IPC sites.
pub fn fillHeaderInSharedMemory(
    header: *Header,
    base_ptr: [*]align(1) const u8,
    image_size: usize,
    lowered: *const LowerIr.Result,
    target_usize: base.target.TargetUsize,
    platform_entrypoints: []const PlatformEntrypoint,
) ImageError!void {
    return fillHeaderInBuffer(header, base_ptr, image_size, lowered, target_usize, platform_entrypoints);
}

/// View an ARC-inserted LIR program in place from a mapped buffer.
///
/// The buffer is treated as read-only by the view — `LirStore` and
/// `layout_mod.Store` are constructed with slices that the interpreter
/// reads but never mutates. Accepting `const` here lets embedders that
/// hold the buffer behind a `const` pointer (e.g. a `FixedBufferAllocator`
/// backed by `gpa.alignedAlloc` whose owning slice is `const`) pass it
/// directly without a manual `@constCast`.
pub fn viewMappedImage(header: *const Header, base_ptr: [*]align(1) const u8, mapped_size: usize) ImageError!ProgramView {
    if (mapped_size < @sizeOf(Header)) return error.InvalidRuntimeImage;

    if (header.magic != MAGIC) return error.InvalidRuntimeImage;
    if (header.format_version != FORMAT_VERSION) return error.UnsupportedRuntimeImageVersion;
    if (header.image_size > mapped_size) return error.InvalidRuntimeImage;

    const target_usize: base.target.TargetUsize = switch (header.target_usize) {
        0 => .u32,
        1 => .u64,
        else => return error.InvalidRuntimeImage,
    };

    // The view path constructs mutable container types (LirStore, Store)
    // whose slice fields are not const, even though the interpreter only
    // reads them. Cast once at the boundary so callers don't have to.
    const mutable_base: [*]align(1) u8 = @constCast(base_ptr);

    return .{
        .store = try header.store.view(mutable_base, @intCast(header.image_size)),
        .layouts = try header.layouts.view(mutable_base, @intCast(header.image_size), target_usize),
        .root_procs = sliceFromRef(LIR.LirProcSpecId, mutable_base, @intCast(header.image_size), header.root_procs),
        .platform_entrypoints = sliceFromRef(PlatformEntrypoint, mutable_base, @intCast(header.image_size), header.platform_entrypoints),
        .target_usize = target_usize,
    };
}

fn arrayRef(base_ptr: [*]align(1) const u8, image_size: usize, slice: anytype) ImageError!ArrayRef {
    if (slice.len == 0) return ArrayRef.empty();

    const base_addr = @intFromPtr(base_ptr);
    const ptr_addr = @intFromPtr(slice.ptr);
    if (ptr_addr < base_addr) return error.InvalidRuntimeImage;

    const offset = ptr_addr - base_addr;
    const byte_len = slice.len * @sizeOf(std.meta.Child(@TypeOf(slice)));
    if (offset + byte_len > image_size) return error.InvalidRuntimeImage;

    return .{
        .offset = @intCast(offset),
        .len = @intCast(slice.len),
        .capacity = @intCast(slice.len),
    };
}

fn multiArrayRef(
    comptime T: type,
    base_ptr: [*]align(1) const u8,
    image_size: usize,
    list: collections.SafeMultiList(T),
) ImageError!ArrayRef {
    if (list.items.capacity == 0) return ArrayRef.empty();

    const base_addr = @intFromPtr(base_ptr);
    const ptr_addr = @intFromPtr(list.items.bytes);
    if (ptr_addr < base_addr) return error.InvalidRuntimeImage;

    const offset = ptr_addr - base_addr;
    const byte_len = std.MultiArrayList(T).capacityInBytes(list.items.capacity);
    if (offset + byte_len > image_size) return error.InvalidRuntimeImage;

    return .{
        .offset = @intCast(offset),
        .len = @intCast(list.items.len),
        .capacity = @intCast(list.items.capacity),
    };
}

fn sliceFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) []T {
    if (ref.len == 0) return &.{};
    debugCheckArrayRef(T, image_size, ref);
    const ptr: [*]T = @ptrCast(@alignCast(base_ptr + @as(usize, @intCast(ref.offset))));
    return ptr[0..@intCast(ref.len)];
}

fn arrayListFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) std.ArrayList(T) {
    const slice = sliceFromRef(T, base_ptr, image_size, ref);
    return .{
        .items = slice,
        .capacity = @intCast(ref.capacity),
    };
}

fn safeListFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) collections.SafeList(T) {
    const slice = sliceFromRef(T, base_ptr, image_size, ref);
    return .{
        .items = .{
            .items = slice,
            .capacity = @intCast(ref.capacity),
        },
    };
}

fn stringLiteralBufferFromRef(base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) base.StringLiteral.Store.Buffer {
    if (ref.capacity == 0) return .{};
    debugCheckByteRef(image_size, ref, @as(usize, @intCast(ref.len)));

    const ptr: [*]align(base.StringLiteral.Store.static_refcount_alignment) u8 = @ptrCast(@alignCast(base_ptr + @as(usize, @intCast(ref.offset))));
    return base.StringLiteral.Store.Buffer.fromMappedSlice(ptr[0..@intCast(ref.len)], @intCast(ref.capacity));
}

fn safeMultiListFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) collections.SafeMultiList(T) {
    if (ref.capacity == 0) return .{ .items = .{} };
    debugCheckByteRef(image_size, ref, std.MultiArrayList(T).capacityInBytes(@intCast(ref.capacity)));
    const ptr: [*]align(@alignOf(T)) u8 = @ptrCast(@alignCast(base_ptr + @as(usize, @intCast(ref.offset))));
    return .{
        .items = .{
            .bytes = ptr,
            .len = @intCast(ref.len),
            .capacity = @intCast(ref.capacity),
        },
    };
}

fn debugCheckArrayRef(comptime T: type, image_size: usize, ref: ArrayRef) void {
    debugCheckByteRef(image_size, ref, @as(usize, @intCast(ref.len)) * @sizeOf(T));
}

fn debugCheckByteRef(image_size: usize, ref: ArrayRef, byte_len: usize) void {
    if (@import("builtin").mode == .Debug) {
        const offset: usize = @intCast(ref.offset);
        if (offset + byte_len > image_size) {
            std.debug.panic("LIR runtime image invariant violated: offset={d} byte_len={d} image_size={d}", .{ offset, byte_len, image_size });
        }
    } else if (@as(usize, @intCast(ref.offset)) + byte_len > image_size) {
        unreachable;
    }
}

test "runtime image declarations are referenced" {
    std.testing.refAllDecls(@This());
}
