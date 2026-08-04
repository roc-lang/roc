//! Shared-memory ARC-inserted LIR image for interpreter-shim execution.
//!
//! The parent process owns checking and post-check compilation. It completes
//! checked modules, lowers directly to LIR, inserts ARC, and then places the
//! exact finalized LIR/layout arrays plus an offset table in shared memory. The
//! child process maps the same object and views those arrays in place; it never
//! reconstructs compiler data.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Program = core.Program;
const GuardedList = collections.GuardedList;

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
/// v11: LocalSpan lengths are u32 for large proc frame-local spans.
/// v12: statements carry virtual inline-scope ids and the image stores the
///      corresponding source-procedure/call-site graph.
/// v13: erased calls and procedures carry explicit packed-argument plans.
pub const FORMAT_VERSION: u32 = 13;

/// Public `ImageError` declaration.
pub const ImageError = error{
    InvalidLirImage,
    UnsupportedLirImageVersion,
};

/// Errors produced while copying finalized LIR into an image buffer.
pub const CopyError = std.mem.Allocator.Error || ImageError;

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
    u32s: ArrayRef,
    erased_call_arg_plans: ArrayRef,
    proc_specs: ArrayRef,
    strings: StringLiteralStoreImage,
    next_synthetic_symbol: u64,
    source_file_bytes: ArrayRef,
    source_file_ends: ArrayRef,
    cf_stmt_locs: ArrayRef,
    cf_stmt_regions: ArrayRef,
    cf_stmt_inline_scopes: ArrayRef,
    inline_scopes: ArrayRef,
    proc_locs: ArrayRef,
    proc_debug_names: ArrayRef,
    local_names: ArrayRef,

    fn fromStore(base_ptr: [*]align(1) const u8, image_size: usize, store: *const LirStore) ImageError!LirStoreImage {
        return .{
            .cf_stmts = try arrayRef(base_ptr, image_size, store.cf_stmts.unsafeRawItemsForView()),
            .cf_switch_branches = try arrayRef(base_ptr, image_size, store.cf_switch_branches.unsafeRawItemsForView()),
            .str_match_steps = try arrayRef(base_ptr, image_size, store.str_match_steps.unsafeRawItemsForView()),
            .str_match_arms = try arrayRef(base_ptr, image_size, store.str_match_arms.unsafeRawItemsForView()),
            .join_points = try arrayRef(base_ptr, image_size, store.join_points.unsafeRawItemsForView()),
            .locals = try arrayRef(base_ptr, image_size, store.locals.unsafeRawItemsForView()),
            .local_ids = try arrayRef(base_ptr, image_size, store.local_ids.unsafeRawItemsForView()),
            .u64s = try arrayRef(base_ptr, image_size, store.u64s.unsafeRawItemsForView()),
            .u32s = try arrayRef(base_ptr, image_size, store.u32s.unsafeRawItemsForView()),
            .erased_call_arg_plans = try arrayRef(base_ptr, image_size, store.erased_call_arg_plans.unsafeRawItemsForView()),
            .proc_specs = try arrayRef(base_ptr, image_size, store.proc_specs.unsafeRawItemsForView()),
            .strings = try StringLiteralStoreImage.fromStore(base_ptr, image_size, &store.strings),
            .next_synthetic_symbol = store.next_synthetic_symbol,
            .source_file_bytes = try arrayRef(base_ptr, image_size, store.source_file_bytes.unsafeRawItemsForView()),
            .source_file_ends = try arrayRef(base_ptr, image_size, store.source_file_ends.unsafeRawItemsForView()),
            .cf_stmt_locs = try arrayRef(base_ptr, image_size, store.cf_stmt_locs.unsafeRawItemsForView()),
            .cf_stmt_regions = try arrayRef(base_ptr, image_size, store.cf_stmt_regions.unsafeRawItemsForView()),
            .cf_stmt_inline_scopes = try arrayRef(base_ptr, image_size, store.cf_stmt_inline_scopes.unsafeRawItemsForView()),
            .inline_scopes = try arrayRef(base_ptr, image_size, store.inline_scopes.unsafeRawItemsForView()),
            .proc_locs = try arrayRef(base_ptr, image_size, store.proc_locs.unsafeRawItemsForView()),
            .proc_debug_names = try arrayRef(base_ptr, image_size, store.proc_debug_names.unsafeRawItemsForView()),
            .local_names = try arrayRef(base_ptr, image_size, store.local_names.unsafeRawItemsForView()),
        };
    }

    fn copyFromStore(
        allocator: std.mem.Allocator,
        base_ptr: [*]align(1) const u8,
        image_capacity: usize,
        store: *const LirStore,
    ) CopyError!LirStoreImage {
        return .{
            .cf_stmts = try copyArrayRef(allocator, base_ptr, image_capacity, store.cf_stmts.unsafeRawItemsForView()),
            .cf_switch_branches = try copyArrayRef(allocator, base_ptr, image_capacity, store.cf_switch_branches.unsafeRawItemsForView()),
            .str_match_steps = try copyArrayRef(allocator, base_ptr, image_capacity, store.str_match_steps.unsafeRawItemsForView()),
            .str_match_arms = try copyArrayRef(allocator, base_ptr, image_capacity, store.str_match_arms.unsafeRawItemsForView()),
            .join_points = try copyArrayRef(allocator, base_ptr, image_capacity, store.join_points.unsafeRawItemsForView()),
            .locals = try copyArrayRef(allocator, base_ptr, image_capacity, store.locals.unsafeRawItemsForView()),
            .local_ids = try copyArrayRef(allocator, base_ptr, image_capacity, store.local_ids.unsafeRawItemsForView()),
            .u64s = try copyArrayRef(allocator, base_ptr, image_capacity, store.u64s.unsafeRawItemsForView()),
            .u32s = try copyArrayRef(allocator, base_ptr, image_capacity, store.u32s.unsafeRawItemsForView()),
            .erased_call_arg_plans = try copyArrayRef(allocator, base_ptr, image_capacity, store.erased_call_arg_plans.unsafeRawItemsForView()),
            .proc_specs = try copyArrayRef(allocator, base_ptr, image_capacity, store.proc_specs.unsafeRawItemsForView()),
            .strings = try StringLiteralStoreImage.copyFromStore(allocator, base_ptr, image_capacity, &store.strings),
            .next_synthetic_symbol = store.next_synthetic_symbol,
            .source_file_bytes = try copyArrayRef(allocator, base_ptr, image_capacity, store.source_file_bytes.unsafeRawItemsForView()),
            .source_file_ends = try copyArrayRef(allocator, base_ptr, image_capacity, store.source_file_ends.unsafeRawItemsForView()),
            .cf_stmt_locs = try copyArrayRef(allocator, base_ptr, image_capacity, store.cf_stmt_locs.unsafeRawItemsForView()),
            .cf_stmt_regions = try copyArrayRef(allocator, base_ptr, image_capacity, store.cf_stmt_regions.unsafeRawItemsForView()),
            .cf_stmt_inline_scopes = try copyArrayRef(allocator, base_ptr, image_capacity, store.cf_stmt_inline_scopes.unsafeRawItemsForView()),
            .inline_scopes = try copyArrayRef(allocator, base_ptr, image_capacity, store.inline_scopes.unsafeRawItemsForView()),
            .proc_locs = try copyArrayRef(allocator, base_ptr, image_capacity, store.proc_locs.unsafeRawItemsForView()),
            .proc_debug_names = try copyArrayRef(allocator, base_ptr, image_capacity, store.proc_debug_names.unsafeRawItemsForView()),
            .local_names = try copyArrayRef(allocator, base_ptr, image_capacity, store.local_names.unsafeRawItemsForView()),
        };
    }

    fn view(self: LirStoreImage, base_ptr: [*]align(1) u8, image_size: usize, allocator: std.mem.Allocator) ImageError!LirStore {
        return .{
            .cf_stmts = try guardedListFromRef(LIR.CFStmt, "LirStore.cf_stmts", base_ptr, image_size, self.cf_stmts),
            .cf_switch_branches = try guardedListFromRef(LIR.CFSwitchBranch, "LirStore.cf_switch_branches", base_ptr, image_size, self.cf_switch_branches),
            .str_match_steps = try guardedListFromRef(LIR.StrMatchStep, "LirStore.str_match_steps", base_ptr, image_size, self.str_match_steps),
            .str_match_arms = try guardedListFromRef(LIR.StrMatchArm, "LirStore.str_match_arms", base_ptr, image_size, self.str_match_arms),
            .join_points = try guardedListFromRef(LIR.JoinPoint, "LirStore.join_points", base_ptr, image_size, self.join_points),
            .locals = try guardedListFromRef(LIR.Local, "LirStore.locals", base_ptr, image_size, self.locals),
            .local_ids = try guardedListFromRef(LIR.LocalId, "LirStore.local_ids", base_ptr, image_size, self.local_ids),
            .u64s = try guardedListFromRef(u64, "LirStore.u64s", base_ptr, image_size, self.u64s),
            .u32s = try guardedListFromRef(u32, "LirStore.u32s", base_ptr, image_size, self.u32s),
            .erased_call_arg_plans = try guardedListFromRef(LIR.ErasedCallArgsPlan, "LirStore.erased_call_arg_plans", base_ptr, image_size, self.erased_call_arg_plans),
            .proc_specs = try guardedListFromRef(LIR.LirProcSpec, "LirStore.proc_specs", base_ptr, image_size, self.proc_specs),
            .strings = try self.strings.view(base_ptr, image_size),
            .string_builder = .{},
            .strings_insertable = false,
            .allocator = allocator,
            .next_synthetic_symbol = self.next_synthetic_symbol,
            .patterns = .empty,
            .pattern_ids = .empty,
            .source_file_bytes = try guardedListFromRef(u8, "LirStore.source_file_bytes", base_ptr, image_size, self.source_file_bytes),
            .source_file_ends = try guardedListFromRef(u32, "LirStore.source_file_ends", base_ptr, image_size, self.source_file_ends),
            .cf_stmt_locs = try guardedListFromRef(base.SourceLoc, "LirStore.cf_stmt_locs", base_ptr, image_size, self.cf_stmt_locs),
            .cf_stmt_regions = try guardedListFromRef(base.Region, "LirStore.cf_stmt_regions", base_ptr, image_size, self.cf_stmt_regions),
            .cf_stmt_inline_scopes = try guardedListFromRef(LIR.InlineScopeId, "LirStore.cf_stmt_inline_scopes", base_ptr, image_size, self.cf_stmt_inline_scopes),
            .inline_scopes = try guardedListFromRef(LIR.InlineScope, "LirStore.inline_scopes", base_ptr, image_size, self.inline_scopes),
            .proc_locs = try guardedListFromRef(base.SourceLoc, "LirStore.proc_locs", base_ptr, image_size, self.proc_locs),
            .proc_debug_names = try guardedListFromRef(LirStore.ProcDebugName, "LirStore.proc_debug_names", base_ptr, image_size, self.proc_debug_names),
            .local_names = try guardedListFromRef(u32, "LirStore.local_names", base_ptr, image_size, self.local_names),
            .current_loc = base.SourceLoc.none,
            .current_region = base.Region.zero(),
            .current_inline_scope = LIR.InlineScopeId.none,
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

    fn copyFromStore(
        allocator: std.mem.Allocator,
        base_ptr: [*]align(1) const u8,
        image_capacity: usize,
        store: *const base.StringLiteral.Store,
    ) CopyError!StringLiteralStoreImage {
        return .{
            .buffer = try copyArrayRef(allocator, base_ptr, image_capacity, store.buffer.items.items),
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

    fn copyFromStore(
        allocator: std.mem.Allocator,
        base_ptr: [*]align(1) const u8,
        image_capacity: usize,
        store: *const layout_mod.Store,
    ) CopyError!LayoutStoreImage {
        return .{
            .layouts = try copyArrayRef(allocator, base_ptr, image_capacity, store.layouts.items.items),
            .resolved_list_layouts = try copyArrayRef(allocator, base_ptr, image_capacity, store.resolved_list_layouts.items),
            .tuple_elems = try copyArrayRef(allocator, base_ptr, image_capacity, store.tuple_elems.items.items),
            .struct_fields = try copyMultiArrayRef(layout_mod.StructField, allocator, base_ptr, image_capacity, store.struct_fields),
            .struct_data = try copyArrayRef(allocator, base_ptr, image_capacity, store.struct_data.items.items),
            .tag_union_variants = try copyMultiArrayRef(layout_mod.TagUnionVariant, allocator, base_ptr, image_capacity, store.tag_union_variants),
            .tag_union_data = try copyArrayRef(allocator, base_ptr, image_capacity, store.tag_union_data.items.items),
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
            .interned_recursive_graphs = std.StringHashMap(layout_mod.Idx).init(allocator),
            .target_usize = target_usize,
        };
    }
};

comptime {
    // The LIR image mirrors these three stores field-for-field. When a
    // serialized field is added to or removed from a store, update the matching
    // `*Image` extern struct, its `fromStore` and `view` methods, and the
    // "LIR image round-trips every populated store field" test at the bottom of
    // this file, then update the expected field count below. A same-build
    // omission (a new store field left out of the image plumbing) is otherwise
    // silent, since `FORMAT_VERSION` only guards cross-version mismatches.
    std.debug.assert(@typeInfo(LirStore).@"struct".fields.len == 30);
    std.debug.assert(@typeInfo(layout_mod.Store).@"struct".fields.len == 12);
    std.debug.assert(@typeInfo(base.StringLiteral.Store).@"struct".fields.len == 1);
}

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

/// Exact copied LIR data awaiting its final image size and header.
pub const CopiedProgram = struct {
    image_capacity: usize,
    root_procs: ArrayRef,
    platform_entrypoints: ArrayRef,
    store: LirStoreImage,
    layouts: LayoutStoreImage,

    pub fn fillHeader(self: CopiedProgram, header: *Header, image_size: usize) ImageError!void {
        if (image_size > self.image_capacity) return error.InvalidLirImage;
        header.* = .{
            .magic = MAGIC,
            .format_version = FORMAT_VERSION,
            .image_size = image_size,
            .root_procs = self.root_procs,
            .platform_entrypoints = self.platform_entrypoints,
            .store = self.store,
            .layouts = self.layouts,
        };
    }
};

/// Copy a finalized ARC-ready LIR program into an independently allocated
/// image buffer. Compiler scratch and intermediate IR stay in their ordinary
/// reclaimable allocator; the image owns only the arrays its mapped consumer
/// reads. `image_capacity` validates every copied pointer; the returned data
/// writes a header only after the caller supplies the allocator's final used
/// size.
pub fn copyProgramIntoBuffer(
    allocator: std.mem.Allocator,
    base_ptr: [*]align(1) const u8,
    image_capacity: usize,
    lowered: *const Program.Result,
    platform_entrypoints: []const PlatformEntrypoint,
) CopyError!CopiedProgram {
    return .{
        .image_capacity = image_capacity,
        .root_procs = try copyArrayRef(allocator, base_ptr, image_capacity, lowered.root_procs.items),
        .platform_entrypoints = try copyArrayRef(allocator, base_ptr, image_capacity, platform_entrypoints),
        .store = try LirStoreImage.copyFromStore(allocator, base_ptr, image_capacity, &lowered.store),
        .layouts = try LayoutStoreImage.copyFromStore(allocator, base_ptr, image_capacity, &lowered.layouts),
    };
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

fn copyArrayRef(
    allocator: std.mem.Allocator,
    base_ptr: [*]align(1) const u8,
    image_capacity: usize,
    source: anytype,
) CopyError!ArrayRef {
    if (source.len == 0) return ArrayRef.empty();
    const T = std.meta.Child(@TypeOf(source));
    const copied = try allocator.dupe(T, source);
    return try arrayRef(base_ptr, image_capacity, copied);
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

fn copyMultiArrayRef(
    comptime T: type,
    allocator: std.mem.Allocator,
    base_ptr: [*]align(1) const u8,
    image_capacity: usize,
    source: collections.SafeMultiList(T),
) CopyError!ArrayRef {
    if (source.items.len == 0) return ArrayRef.empty();
    const copied = try source.clone(allocator);
    return try multiArrayRef(T, base_ptr, image_capacity, copied);
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

fn guardedListFromRef(
    comptime T: type,
    comptime name: []const u8,
    base_ptr: [*]align(1) u8,
    image_size: usize,
    ref: ArrayRef,
) ImageError!GuardedList.List(T, name) {
    return GuardedList.List(T, name).fromArrayList(try arrayListFromRef(T, base_ptr, image_size, ref));
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

    const ptr: [*]u8 = @ptrCast(base_ptr + try checkedOffset(ref));
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

/// The 20 `LirStore` array-backed lists serialized as `ArrayRef`s, in the order
/// they appear in `LirStoreImage`. `strings` (a sub-image) and the scalar
/// `next_synthetic_symbol` are serialized too but exercised separately below.
const serialized_guarded_fields = [_][]const u8{
    "cf_stmts",
    "cf_switch_branches",
    "str_match_steps",
    "str_match_arms",
    "join_points",
    "locals",
    "local_ids",
    "u64s",
    "u32s",
    "erased_call_arg_plans",
    "proc_specs",
    "source_file_bytes",
    "source_file_ends",
    "cf_stmt_locs",
    "cf_stmt_regions",
    "cf_stmt_inline_scopes",
    "inline_scopes",
    "proc_locs",
    "proc_debug_names",
    "local_names",
};

test "LIR image copies and round-trips every populated store field" {
    const gpa = std.testing.allocator;

    // Build the compiler result outside the image buffer so this test proves
    // copying transfers every mapped field instead of retaining source
    // pointers.
    var source_arena = std.heap.ArenaAllocator.init(gpa);
    defer source_arena.deinit();
    const source_allocator = source_arena.allocator();

    // The copied image has independent contiguous storage.
    const buffer = try gpa.alignedAlloc(u8, .@"16", 1 << 20);
    defer gpa.free(buffer);
    var fba_state = std.heap.FixedBufferAllocator.init(buffer);
    const fba = fba_state.allocator();

    const target_usize = base.target.TargetUsize.native;

    const h = struct {
        /// Allocate `count` elements of `T` and fill their raw bytes with a
        /// per-field-distinctive, per-index pattern so a dropped or swapped
        /// field (same or different element type) is detectable after view.
        fn distinct(comptime T: type, alloc: std.mem.Allocator, count: usize, seed: u8) std.mem.Allocator.Error![]T {
            const slice = try alloc.alloc(T, count);
            const bytes = std.mem.sliceAsBytes(slice);
            for (bytes, 0..) |*b, i| b.* = seed +% @as(u8, @truncate(i));
            return slice;
        }
        /// Build a populated `GuardedList` for a `LirStore` field of type `FieldT`.
        fn guarded(comptime FieldT: type, alloc: std.mem.Allocator, count: usize, seed: u8) std.mem.Allocator.Error!FieldT {
            const T = std.meta.Child(FieldT.Slice);
            const slice = try distinct(T, alloc, count, seed);
            return FieldT.fromArrayList(.{ .items = slice, .capacity = count });
        }
        /// Build a populated `SafeList(T)` backed by the fixed buffer.
        fn safeList(comptime T: type, alloc: std.mem.Allocator, count: usize, seed: u8) std.mem.Allocator.Error!collections.SafeList(T) {
            const slice = try distinct(T, alloc, count, seed);
            return .{ .items = .{ .items = slice, .capacity = count } };
        }
        /// Build a populated `SafeMultiList(T)` backed by the fixed buffer.
        fn multiList(comptime T: type, alloc: std.mem.Allocator, count: usize, seed: u8) std.mem.Allocator.Error!collections.SafeMultiList(T) {
            var mal: std.MultiArrayList(T) = .{};
            try mal.resize(alloc, count);
            const total = std.MultiArrayList(T).capacityInBytes(mal.capacity);
            for (mal.bytes[0..total], 0..) |*b, i| b.* = seed +% @as(u8, @truncate(i));
            return .{ .items = mal };
        }
        /// Assert two byte spans are equal and non-empty.
        fn expectBytesEq(a: []const u8, b: []const u8) error{ TestExpectedEqual, TestUnexpectedResult }!void {
            try std.testing.expect(a.len > 0);
            try std.testing.expectEqualSlices(u8, a, b);
        }
    };

    // A LirStore with every serialized list populated distinctively.
    var store = LirStore.init(source_allocator);
    inline for (serialized_guarded_fields, 0..) |fname, i| {
        @field(store, fname) = try h.guarded(@FieldType(LirStore, fname), source_allocator, 2 + i, @intCast(0x20 + i));
    }
    store.next_synthetic_symbol = 0x0123_4567_89ab_cdef;
    store.strings = .{ .buffer = .{ .items = .{ .items = try h.distinct(u8, source_allocator, 24, 0x90), .capacity = 24 } } };

    // A layout Store with every serialized list populated distinctively. Only
    // the seven array-backed fields are serialized; the interning caches are not
    // read by `fromStore`, so they are left undefined here.
    var layouts = layout_mod.Store{
        .allocator = gpa,
        .layouts = try h.safeList(layout_mod.Layout, source_allocator, 3, 0x40),
        .resolved_list_layouts = .{ .items = try h.distinct(?layout_mod.Idx, source_allocator, 4, 0x50), .capacity = 4 },
        .tuple_elems = try h.safeList(layout_mod.Idx, source_allocator, 5, 0x60),
        .struct_fields = try h.multiList(layout_mod.StructField, source_allocator, 6, 0x70),
        .struct_data = try h.safeList(layout_mod.StructData, source_allocator, 7, 0x80),
        .tag_union_variants = try h.multiList(layout_mod.TagUnionVariant, source_allocator, 8, 0x88),
        .tag_union_data = try h.safeList(layout_mod.TagUnionData, source_allocator, 9, 0xa0),
        .interned_layouts = undefined,
        .scratch_intern_key = undefined,
        .interned_recursive_graphs = undefined,
        .target_usize = target_usize,
    };

    const root_procs = try h.distinct(LIR.LirProcSpecId, source_allocator, 3, 0xb0);
    const entrypoints = try h.distinct(PlatformEntrypoint, source_allocator, 2, 0xc0);

    const base_ptr = buffer.ptr;
    const header = try fba.create(Header);
    var lowered: Program.Result = undefined;
    lowered.store = store;
    lowered.layouts = layouts;
    lowered.root_procs = .{ .items = root_procs, .capacity = root_procs.len };
    const copied = try copyProgramIntoBuffer(fba, base_ptr, buffer.len, &lowered, entrypoints);
    try copied.fillHeader(header, fba_state.end_index);

    // View back over the same buffer.
    var view = try viewMappedImageWithAllocator(header, base_ptr, buffer.len, target_usize, gpa);
    defer view.layouts.interned_layouts.deinit();
    defer view.layouts.scratch_intern_key.deinit(gpa);

    // Every serialized guarded list must round-trip byte-for-byte. A field
    // omitted from `fromStore`/`view` would read back as empty and fail here.
    inline for (serialized_guarded_fields) |fname| {
        const a = @field(store, fname).unsafeRawItemsForView();
        const b = @field(view.store, fname).unsafeRawItemsForView();
        try std.testing.expectEqual(a.len, b.len);
        try h.expectBytesEq(std.mem.sliceAsBytes(a), std.mem.sliceAsBytes(b));
    }

    // Scalar and sub-image fields.
    try std.testing.expectEqual(@as(u64, 0x0123_4567_89ab_cdef), view.store.next_synthetic_symbol);
    try h.expectBytesEq(store.strings.buffer.items.items, view.store.strings.buffer.items.items);

    // `patterns`/`pattern_ids` carry no data in statement-only LIR (nothing
    // lowers into the LIR-level pattern lists), so the image intentionally omits
    // them and `view` restores them empty. Assert that intent explicitly rather
    // than round-tripping populated data.
    try std.testing.expectEqual(@as(usize, 0), view.store.patterns.len());
    try std.testing.expectEqual(@as(usize, 0), view.store.pattern_ids.len());

    // Ambient lowering state is reset by `view`; it is not image data.
    try std.testing.expectEqual(base.SourceLoc.none, view.store.current_loc);
    try std.testing.expectEqual(base.Region.zero(), view.store.current_region);
    try std.testing.expectEqual(LIR.InlineScopeId.none, view.store.current_inline_scope);
    // A viewed image is read-only, so string insertion is disabled.
    try std.testing.expectEqual(false, view.store.strings_insertable);

    // Layout store: seven serialized lists plus the view-supplied target width.
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.layouts.items.items),
        std.mem.sliceAsBytes(view.layouts.layouts.items.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.resolved_list_layouts.items),
        std.mem.sliceAsBytes(view.layouts.resolved_list_layouts.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.tuple_elems.items.items),
        std.mem.sliceAsBytes(view.layouts.tuple_elems.items.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.struct_data.items.items),
        std.mem.sliceAsBytes(view.layouts.struct_data.items.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.tag_union_data.items.items),
        std.mem.sliceAsBytes(view.layouts.tag_union_data.items.items),
    );
    {
        const T = layout_mod.StructField;
        const orig = &layouts.struct_fields.items;
        const seen = &view.layouts.struct_fields.items;
        try std.testing.expectEqual(orig.len, seen.len);
        try h.expectBytesEq(
            orig.bytes[0..std.MultiArrayList(T).capacityInBytes(orig.capacity)],
            seen.bytes[0..std.MultiArrayList(T).capacityInBytes(seen.capacity)],
        );
    }
    {
        const T = layout_mod.TagUnionVariant;
        const orig = &layouts.tag_union_variants.items;
        const seen = &view.layouts.tag_union_variants.items;
        try std.testing.expectEqual(orig.len, seen.len);
        try h.expectBytesEq(
            orig.bytes[0..std.MultiArrayList(T).capacityInBytes(orig.capacity)],
            seen.bytes[0..std.MultiArrayList(T).capacityInBytes(seen.capacity)],
        );
    }
    try std.testing.expectEqual(target_usize, view.layouts.target_usize);
    try std.testing.expectEqual(target_usize, view.target_usize);

    // Header-level array refs.
    try std.testing.expectEqual(@as(usize, 3), view.root_procs.len);
    try std.testing.expectEqual(@as(usize, 2), view.platform_entrypoints.len);
    try h.expectBytesEq(std.mem.sliceAsBytes(root_procs), std.mem.sliceAsBytes(view.root_procs));
    try h.expectBytesEq(std.mem.sliceAsBytes(entrypoints), std.mem.sliceAsBytes(view.platform_entrypoints));
}
