//! Shared-memory ARC-inserted LIR image for interpreter-shim execution.
//!
//! The parent process owns checking and post-check compilation. It completes
//! checked modules, lowers directly to LIR, inserts ARC, and then writes a small
//! offset table into the existing shared-memory allocator. The child process maps
//! the same shared-memory object and views the LIR/layout arrays in place; it
//! never reconstructs compiler data.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Program = core.Program;

/// Public `MAGIC` declaration.
pub const MAGIC: u32 = 0x52494c52; // "RLIR" in little-endian bytes.
/// Public `FORMAT_VERSION` declaration.
/// v5: added LayoutTag.ptr and the TRMC LowLevel ops (ptr_alloca,
/// box_alloc_zeroed, ptr_store, ptr_load, ptr_cast).
/// v6: string-pattern captures are explicit borrowed Str views.
/// v7: string-pattern match sets add grouped arm storage.
/// v8: LIR statements carry explicit checked source regions for diagnostics.
/// v9: image is pointer-width independent; the target is supplied at view time
///     rather than recorded in the header.
/// v10: LIR proc specs carry explicit native stack-probe requirements.
pub const FORMAT_VERSION: u32 = 10;

/// Public `ImageError` declaration.
pub const ImageError = error{
    InvalidLirImage,
    UnsupportedLirImageVersion,
};

/// Direct interpreter entrypoint written by the parent.
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
///
/// The image is pointer-width independent: the layout store carries both widths'
/// sizes and offsets and the LIR op stream makes no width-dependent decisions,
/// so the recorded bytes do not encode a target. The consumer supplies the
/// width it is resolving for when it views the image (see `viewMappedImage`).
pub const Header = extern struct {
    magic: u32,
    format_version: u32,
    image_size: u64,
    _padding: [8]u8 = [_]u8{0} ** 8,
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
    str_match_steps: ArrayRef,
    str_match_arms: ArrayRef,
    join_points: ArrayRef,
    locals: ArrayRef,
    local_ids: ArrayRef,
    u64s: ArrayRef,
    proc_specs: ArrayRef,
    strings: StringLiteralStoreImage,
    next_synthetic_symbol: u64,
    source_file_bytes: ArrayRef,
    source_file_ends: ArrayRef,
    cf_stmt_locs: ArrayRef,
    cf_stmt_regions: ArrayRef,
    proc_locs: ArrayRef,
    proc_debug_names: ArrayRef,
    local_names: ArrayRef,

    fn fromStore(base_ptr: [*]align(1) const u8, image_size: usize, store: *const LirStore) ImageError!LirStoreImage {
        return .{
            .cf_stmts = try arrayRef(base_ptr, image_size, store.cf_stmts.items),
            .cf_switch_branches = try arrayRef(base_ptr, image_size, store.cf_switch_branches.items),
            .str_match_steps = try arrayRef(base_ptr, image_size, store.str_match_steps.items),
            .str_match_arms = try arrayRef(base_ptr, image_size, store.str_match_arms.items),
            .join_points = try arrayRef(base_ptr, image_size, store.join_points.items),
            .locals = try arrayRef(base_ptr, image_size, store.locals.items),
            .local_ids = try arrayRef(base_ptr, image_size, store.local_ids.items),
            .u64s = try arrayRef(base_ptr, image_size, store.u64s.items),
            .proc_specs = try arrayRef(base_ptr, image_size, store.proc_specs.items),
            .strings = try StringLiteralStoreImage.fromStore(base_ptr, image_size, &store.strings),
            .next_synthetic_symbol = store.next_synthetic_symbol,
            .source_file_bytes = try arrayRef(base_ptr, image_size, store.source_file_bytes.items),
            .source_file_ends = try arrayRef(base_ptr, image_size, store.source_file_ends.items),
            .cf_stmt_locs = try arrayRef(base_ptr, image_size, store.cf_stmt_locs.items),
            .cf_stmt_regions = try arrayRef(base_ptr, image_size, store.cf_stmt_regions.items),
            .proc_locs = try arrayRef(base_ptr, image_size, store.proc_locs.items),
            .proc_debug_names = try arrayRef(base_ptr, image_size, store.proc_debug_names.items),
            .local_names = try arrayRef(base_ptr, image_size, store.local_names.items),
        };
    }

    fn view(self: LirStoreImage, base_ptr: [*]align(1) u8, image_size: usize, allocator: std.mem.Allocator) ImageError!LirStore {
        return .{
            .cf_stmts = try arrayListFromRef(LIR.CFStmt, base_ptr, image_size, self.cf_stmts),
            .cf_switch_branches = try arrayListFromRef(LIR.CFSwitchBranch, base_ptr, image_size, self.cf_switch_branches),
            .str_match_steps = try arrayListFromRef(LIR.StrMatchStep, base_ptr, image_size, self.str_match_steps),
            .str_match_arms = try arrayListFromRef(LIR.StrMatchArm, base_ptr, image_size, self.str_match_arms),
            .join_points = try arrayListFromRef(LIR.JoinPoint, base_ptr, image_size, self.join_points),
            .locals = try arrayListFromRef(LIR.Local, base_ptr, image_size, self.locals),
            .local_ids = try arrayListFromRef(LIR.LocalId, base_ptr, image_size, self.local_ids),
            .u64s = try arrayListFromRef(u64, base_ptr, image_size, self.u64s),
            .proc_specs = try arrayListFromRef(LIR.LirProcSpec, base_ptr, image_size, self.proc_specs),
            .strings = try self.strings.view(base_ptr, image_size),
            .string_builder = .{},
            .strings_insertable = false,
            .allocator = allocator,
            .next_synthetic_symbol = self.next_synthetic_symbol,
            .patterns = std.ArrayList(LIR.LirPattern).empty,
            .pattern_ids = std.ArrayList(LIR.LirPatternId).empty,
            .source_file_bytes = try arrayListFromRef(u8, base_ptr, image_size, self.source_file_bytes),
            .source_file_ends = try arrayListFromRef(u32, base_ptr, image_size, self.source_file_ends),
            .cf_stmt_locs = try arrayListFromRef(base.SourceLoc, base_ptr, image_size, self.cf_stmt_locs),
            .cf_stmt_regions = try arrayListFromRef(base.Region, base_ptr, image_size, self.cf_stmt_regions),
            .proc_locs = try arrayListFromRef(base.SourceLoc, base_ptr, image_size, self.proc_locs),
            .proc_debug_names = try arrayListFromRef(LirStore.ProcDebugName, base_ptr, image_size, self.proc_debug_names),
            .local_names = try arrayListFromRef(u32, base_ptr, image_size, self.local_names),
            .current_loc = base.SourceLoc.none,
            .current_region = base.Region.zero(),
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
            .buffer = try stringLiteralBufferFromRef(base_ptr, image_size, self.buffer),
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
        allocator: std.mem.Allocator,
    ) ImageError!layout_mod.Store {
        return .{
            .allocator = allocator,
            .layouts = try safeListFromRef(layout_mod.Layout, base_ptr, image_size, self.layouts),
            .resolved_list_layouts = try arrayListFromRef(?layout_mod.Idx, base_ptr, image_size, self.resolved_list_layouts),
            .tuple_elems = try safeListFromRef(layout_mod.Idx, base_ptr, image_size, self.tuple_elems),
            .struct_fields = try safeMultiListFromRef(layout_mod.StructField, base_ptr, image_size, self.struct_fields),
            .struct_data = try safeListFromRef(layout_mod.StructData, base_ptr, image_size, self.struct_data),
            .tag_union_variants = try safeMultiListFromRef(layout_mod.TagUnionVariant, base_ptr, image_size, self.tag_union_variants),
            .tag_union_data = try safeListFromRef(layout_mod.TagUnionData, base_ptr, image_size, self.tag_union_data),
            .interned_layouts = std.StringHashMap(layout_mod.Idx).init(allocator),
            .scratch_intern_key = .empty,
            .target_usize = target_usize,
        };
    }
};

/// Fill the reserved LIR image header in a contiguous buffer.
///
/// `lowered` must already have been allocated from an allocator that owns
/// the buffer at `base_ptr` (the buffer must contain every pointer reachable
/// from `lowered`). This function only installs offset metadata — it does not
/// copy data.
///
/// This is the IPC-agnostic variant. Use it for in-process embedders that
/// place the LIR image in a plain arena instead of shared memory.
pub fn fillHeaderInBuffer(
    header: *Header,
    base_ptr: [*]align(1) const u8,
    image_size: usize,
    lowered: *const Program.Result,
    platform_entrypoints: []const PlatformEntrypoint,
) ImageError!void {
    header.* = .{
        .magic = MAGIC,
        .format_version = FORMAT_VERSION,
        .image_size = image_size,
        .root_procs = try arrayRef(base_ptr, image_size, lowered.root_procs.items),
        .platform_entrypoints = try arrayRef(base_ptr, image_size, platform_entrypoints),
        .store = try LirStoreImage.fromStore(base_ptr, image_size, &lowered.store),
        .layouts = try LayoutStoreImage.fromStore(base_ptr, image_size, &lowered.layouts),
    };
}

/// Fill the reserved LIR image header in the existing shared-memory mapping.
///
/// `lowered` must already have been allocated with the shared-memory allocator
/// associated with `base_ptr`; this function only installs offset metadata.
///
/// Thin wrapper over `fillHeaderInBuffer` — kept for naming clarity at IPC sites.
pub fn fillHeaderInSharedMemory(
    header: *Header,
    base_ptr: [*]align(1) const u8,
    image_size: usize,
    lowered: *const Program.Result,
    platform_entrypoints: []const PlatformEntrypoint,
) ImageError!void {
    return fillHeaderInBuffer(header, base_ptr, image_size, lowered, platform_entrypoints);
}

/// View an ARC-inserted LIR program in place from a mapped buffer.
///
/// The buffer is treated as read-only by the view — `LirStore` and
/// `layout_mod.Store` are constructed with slices that the interpreter
/// reads but never mutates. Accepting `const` here lets embedders that
/// hold the buffer behind a `const` pointer (e.g. a `FixedBufferAllocator`
/// backed by `gpa.alignedAlloc` whose owning slice is `const`) pass it
/// directly without a manual `@constCast`.
pub fn viewMappedImage(header: *const Header, base_ptr: [*]align(1) const u8, mapped_size: usize, target_usize: base.target.TargetUsize) ImageError!ProgramView {
    return viewMappedImageWithAllocator(header, base_ptr, mapped_size, target_usize, base.defaultGpa());
}

/// View an ARC-inserted LIR program in place from a mapped buffer using the
/// provided allocator for any scratch data owned by reconstructed stores.
///
/// The image contents (LIR op stream and layout store) are pointer-width
/// independent, so the caller supplies the width to resolve layout sizes,
/// offsets, and alignments for. The same image bytes can be viewed for either
/// width — e.g. a cross-width cache reused by both a native interpreter and a
/// 32-bit codegen backend.
pub fn viewMappedImageWithAllocator(
    header: *const Header,
    base_ptr: [*]align(1) const u8,
    mapped_size: usize,
    target_usize: base.target.TargetUsize,
    allocator: std.mem.Allocator,
) ImageError!ProgramView {
    if (mapped_size < @sizeOf(Header)) return error.InvalidLirImage;

    if (header.magic != MAGIC) return error.InvalidLirImage;
    if (header.format_version != FORMAT_VERSION) return error.UnsupportedLirImageVersion;
    if (header.image_size > mapped_size) return error.InvalidLirImage;

    // The view path constructs mutable container types (LirStore, Store)
    // whose slice fields are not const, even though the interpreter only
    // reads them. Cast once at the boundary so callers don't have to.
    const mutable_base: [*]align(1) u8 = @constCast(base_ptr);

    return .{
        .store = try header.store.view(mutable_base, @intCast(header.image_size), allocator),
        .layouts = try header.layouts.view(mutable_base, @intCast(header.image_size), target_usize, allocator),
        .root_procs = try sliceFromRef(LIR.LirProcSpecId, mutable_base, @intCast(header.image_size), header.root_procs),
        .platform_entrypoints = try sliceFromRef(PlatformEntrypoint, mutable_base, @intCast(header.image_size), header.platform_entrypoints),
        .target_usize = target_usize,
    };
}

fn arrayRef(base_ptr: [*]align(1) const u8, image_size: usize, slice: anytype) ImageError!ArrayRef {
    if (slice.len == 0) return ArrayRef.empty();

    const base_addr = @intFromPtr(base_ptr);
    const ptr_addr = @intFromPtr(slice.ptr);
    if (ptr_addr < base_addr) return error.InvalidLirImage;

    const offset = ptr_addr - base_addr;
    const byte_len = slice.len * @sizeOf(std.meta.Child(@TypeOf(slice)));
    if (offset + byte_len > image_size) return error.InvalidLirImage;

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
    if (ptr_addr < base_addr) return error.InvalidLirImage;

    const offset = ptr_addr - base_addr;
    const byte_len = std.MultiArrayList(T).capacityInBytes(list.items.capacity);
    if (offset + byte_len > image_size) return error.InvalidLirImage;

    return .{
        .offset = @intCast(offset),
        .len = @intCast(list.items.len),
        .capacity = @intCast(list.items.capacity),
    };
}

fn sliceFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError![]T {
    if (ref.len == 0) return &.{};
    const len = try checkSliceRef(T, image_size, ref);
    const ptr: [*]T = @ptrCast(@alignCast(base_ptr + try checkedOffset(ref)));
    return ptr[0..len];
}

fn arrayListFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError!std.ArrayList(T) {
    const len, const capacity = try checkListRef(T, image_size, ref);
    const ptr: [*]T = @ptrCast(@alignCast(base_ptr + try checkedOffset(ref)));
    return .{
        .items = ptr[0..len],
        .capacity = capacity,
    };
}

fn safeListFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError!collections.SafeList(T) {
    const list = try arrayListFromRef(T, base_ptr, image_size, ref);
    return .{
        .items = .{
            .items = list.items,
            .capacity = list.capacity,
        },
    };
}

fn stringLiteralBufferFromRef(base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError!base.StringLiteral.Store.Buffer {
    const len, const capacity = try checkByteListRef(image_size, ref);
    if (capacity == 0) return .{};

    const ptr: [*]align(base.StringLiteral.Store.static_refcount_alignment) u8 = @ptrCast(@alignCast(base_ptr + try checkedOffset(ref)));
    return base.StringLiteral.Store.Buffer.fromMappedSlice(ptr[0..len], capacity);
}

fn safeMultiListFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError!collections.SafeMultiList(T) {
    const len = std.math.cast(usize, ref.len) orelse return error.InvalidLirImage;
    const capacity = std.math.cast(usize, ref.capacity) orelse return error.InvalidLirImage;
    if (len > capacity) return error.InvalidLirImage;
    if (capacity == 0) return .{ .items = .{} };
    try checkByteRef(image_size, ref, std.MultiArrayList(T).capacityInBytes(capacity));
    const ptr: [*]align(@alignOf(T)) u8 = @ptrCast(@alignCast(base_ptr + try checkedOffset(ref)));
    return .{
        .items = .{
            .bytes = ptr,
            .len = len,
            .capacity = capacity,
        },
    };
}

fn checkSliceRef(comptime T: type, image_size: usize, ref: ArrayRef) ImageError!usize {
    const len = std.math.cast(usize, ref.len) orelse return error.InvalidLirImage;
    const byte_len = std.math.mul(usize, len, @sizeOf(T)) catch return error.InvalidLirImage;
    try checkByteRef(image_size, ref, byte_len);
    return len;
}

fn checkListRef(comptime T: type, image_size: usize, ref: ArrayRef) ImageError!struct { usize, usize } {
    const len = std.math.cast(usize, ref.len) orelse return error.InvalidLirImage;
    const capacity = std.math.cast(usize, ref.capacity) orelse return error.InvalidLirImage;
    if (len > capacity) return error.InvalidLirImage;
    const byte_len = std.math.mul(usize, capacity, @sizeOf(T)) catch return error.InvalidLirImage;
    try checkByteRef(image_size, ref, byte_len);
    return .{ len, capacity };
}

fn checkByteListRef(image_size: usize, ref: ArrayRef) ImageError!struct { usize, usize } {
    const len = std.math.cast(usize, ref.len) orelse return error.InvalidLirImage;
    const capacity = std.math.cast(usize, ref.capacity) orelse return error.InvalidLirImage;
    if (len > capacity) return error.InvalidLirImage;
    try checkByteRef(image_size, ref, capacity);
    return .{ len, capacity };
}

fn checkByteRef(image_size: usize, ref: ArrayRef, byte_len: usize) ImageError!void {
    const offset = try checkedOffset(ref);
    if (offset > image_size) return error.InvalidLirImage;
    if (byte_len > image_size - offset) return error.InvalidLirImage;
}

fn checkedOffset(ref: ArrayRef) ImageError!usize {
    return std.math.cast(usize, ref.offset) orelse error.InvalidLirImage;
}

test "LIR image declarations are referenced" {
    std.testing.refAllDecls(@This());
}
