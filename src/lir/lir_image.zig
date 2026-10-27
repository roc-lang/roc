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
/// v14: LocalSpan lengths are u32 for large proc frame-local spans.
/// v15: SafeMultiList layout tables use portable typed column arrays.
/// v20: statements carry virtual inline-scope ids and the image stores the
///      corresponding source-procedure/call-site graph.
/// v21: erased calls and procedures carry explicit packed-argument plans.
/// v22: combines the independent v20 and v21 image changes.
/// v24: source file table entries carry package-qualified module identities
///      alongside display names.
/// v25: integer arithmetic uses explicit behavior-family operations.
/// v27: `expect` statements optionally carry a test observation site.
/// v29: procedures and calls carry producer-authored self-tail proofs.
/// v30: SIMD byte alignment carries a proven constant count, alongside v29's
///      self-tail proofs.
/// v31: frozen static values and explicit callable/data relocations.
/// v32: procedure specs carry content identities.
/// v34: procedure specs carry producer-local native code revisions.
/// v35: statements carry an explicit origin kind (`LIR.OriginKind`).
/// v36: removing `num_round` changes the numeric IDs of later LowLevel ops.
/// v37: numeric `*_from_str_prefix`/`*_from_utf8_prefix` ops renumber later LowLevel ops.
/// v38: explicit-endian wide-UTF decoding ops on the wide-UTF branch.
/// v39: `crash` statements record whether checking rejected the code they stand for.
/// v40: source file table entries carry their module's content identity.
/// v41: `proc_ref` is the `LIR.LiteralValue` tag that follows `bytes_literal`.
/// v42: wide-UTF decoding ops renumber later LowLevel ops.
/// v45: layout representation fields are sized for practice, and resolved list
///      layouts are plain layout indices.
/// v51: statements keep their rows out of line, and the store records its
///      lowering modules' keys.
/// v54: combines the independent v51-v53 image changes.
/// v57: images carry only specialized LIR tables.
pub const FORMAT_VERSION: u32 = 57;
const StaticDataImage = @import("lir_image_static_data.zig").Schema(@This());

/// Public `ImageError` declaration.
pub const ImageError = error{
    InvalidLirImage,
    UnsupportedLirImageVersion,
};

/// Errors produced while reconstructing a mapped image view.
pub const ViewError = ImageError || std.mem.Allocator.Error;

/// Errors produced while copying finalized LIR into an image buffer.
pub const CopyError = std.mem.Allocator.Error || ImageError;

/// What the bytes of a copied image are required to be a function of. The
/// producer of an image states this explicitly; it is never inferred from the
/// allocator or the buffer the image is copied into.
pub const ByteContract = enum {
    /// The image lives in shared or process memory for one run and is read
    /// only through typed views, which never observe a byte no value defines.
    /// Its tables are copied verbatim, so such a byte holds whatever the
    /// source store held. Nothing may hash, compare, or write these bytes to
    /// a file.
    mapped,
    /// The image's bytes are written into an artifact that outlives the
    /// compiler process, such as an executable with an embedded interpreter
    /// image. Every table byte is a function of the program's logical
    /// contents: the image's copy of each scrubbable item is canonicalized,
    /// and the source store is not modified. The producer supplies a
    /// zero-filled image buffer, which defines the alignment gaps between the
    /// image's allocations.
    persisted,
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
/// LIR/layout arrays are pointer-width independent. Frozen static bytes carry
/// an explicit pointer width, which the consumer must match when present.
/// Names, bytes, and relocation rows are image-relative offsets, never pointers.
pub const Header = extern struct {
    magic: u32,
    format_version: u32,
    image_size: u64,
    _padding: [8]u8 = @as([8]u8, @splat(0)),
    root_procs: ArrayRef,
    platform_entrypoints: ArrayRef,
    store: LirStoreImage,
    layouts: LayoutStoreImage,
    static_data: ArrayRef = .empty(),
    static_data_value_count: u32 = 0,
    static_data_pointer_bytes: u32 = 0,
};

/// A child-side view over mapped shared memory. Most storage remains mapped,
/// while compact layout columns are reconstructed with `scratch_allocator`.
pub const ProgramView = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: []LIR.LirProcSpecId,
    platform_entrypoints: []PlatformEntrypoint,
    target_usize: base.target.TargetUsize,
    scratch_allocator: std.mem.Allocator,
    static_data: []Program.StaticDataExport,
    static_data_value_count: u32,

    pub fn deinit(self: *ProgramView) void {
        deinitViewedLayouts(&self.layouts, self.scratch_allocator);
        StaticDataImage.deinit(self.scratch_allocator, self.static_data);
        self.* = undefined;
    }
};

/// Public `LirStoreImage` declaration.
pub const LirStoreImage = extern struct {
    runtime_contract: u64,
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
    stmt_rows: [LirStore.stmt_row_column_count]ArrayRef,
    proc_specs: ArrayRef,
    strings: StringLiteralStoreImage,
    next_synthetic_symbol: u64,
    source_file_bytes: ArrayRef,
    source_file_ends: ArrayRef,
    source_file_qualified_bytes: ArrayRef,
    source_file_qualified_ends: ArrayRef,
    source_file_identities: ArrayRef,
    lowering_module_keys: ArrayRef,
    cf_stmt_sources: ArrayRef,
    stmt_sources: ArrayRef,
    cf_stmt_inline_scopes: ArrayRef,
    cf_stmt_origin_kinds: ArrayRef,
    inline_scopes: ArrayRef,
    proc_locs: ArrayRef,
    proc_debug_names: ArrayRef,
    local_names: ArrayRef,

    fn fromStore(encoder: anytype, store: *const LirStore) @TypeOf(encoder).Error!LirStoreImage {
        std.debug.assert(store.tail_call_builder == null);
        std.debug.assert(store.proc_rewrite == null and store.body_coordinator == null);
        return .{
            .runtime_contract = @backingInt(store.runtime_contract),
            .cf_stmts = try encoder.ref(store.cf_stmts.unsafeRawItemsForView()),
            .cf_switch_branches = try encoder.ref(store.cf_switch_branches.unsafeRawItemsForView()),
            .str_match_steps = try encoder.ref(store.str_match_steps.unsafeRawItemsForView()),
            .str_match_arms = try encoder.ref(store.str_match_arms.unsafeRawItemsForView()),
            .join_points = try encoder.ref(store.join_points.unsafeRawItemsForView()),
            .locals = try encoder.ref(store.locals.unsafeRawItemsForView()),
            .local_ids = try encoder.ref(store.local_ids.unsafeRawItemsForView()),
            .u64s = try encoder.ref(store.u64s.unsafeRawItemsForView()),
            .u32s = try encoder.ref(store.u32s.unsafeRawItemsForView()),
            .erased_call_arg_plans = try encoder.ref(store.erased_call_arg_plans.unsafeRawItemsForView()),
            .stmt_rows = try stmtRowRefs(encoder, store),
            .proc_specs = try encoder.ref(store.proc_specs.unsafeRawItemsForView()),
            .strings = try StringLiteralStoreImage.fromStore(encoder, &store.strings),
            .next_synthetic_symbol = store.next_synthetic_symbol,
            .source_file_bytes = try encoder.ref(store.source_file_bytes.unsafeRawItemsForView()),
            .source_file_ends = try encoder.ref(store.source_file_ends.unsafeRawItemsForView()),
            .source_file_qualified_bytes = try encoder.ref(store.source_file_qualified_bytes.unsafeRawItemsForView()),
            .source_file_qualified_ends = try encoder.ref(store.source_file_qualified_ends.unsafeRawItemsForView()),
            .source_file_identities = try encoder.ref(store.source_file_identities.unsafeRawItemsForView()),
            .lowering_module_keys = try encoder.ref(store.lowering_module_keys.unsafeRawItemsForView()),
            .cf_stmt_sources = try encoder.ref(store.cf_stmt_sources.unsafeRawItemsForView()),
            .stmt_sources = try encoder.ref(store.stmt_sources.unsafeRawItemsForView()),
            .cf_stmt_inline_scopes = try encoder.ref(store.cf_stmt_inline_scopes.unsafeRawItemsForView()),
            .cf_stmt_origin_kinds = try encoder.ref(store.cf_stmt_origin_kinds.unsafeRawItemsForView()),
            .inline_scopes = try encoder.ref(store.inline_scopes.unsafeRawItemsForView()),
            .proc_locs = try encoder.ref(store.proc_locs.unsafeRawItemsForView()),
            .proc_debug_names = try encoder.ref(store.proc_debug_names.unsafeRawItemsForView()),
            .local_names = try encoder.ref(store.local_names.unsafeRawItemsForView()),
        };
    }

    fn stmtRowRefs(encoder: anytype, store: *const LirStore) @TypeOf(encoder).Error![LirStore.stmt_row_column_count]ArrayRef {
        var refs: [LirStore.stmt_row_column_count]ArrayRef = undefined;
        inline for (0..LirStore.stmt_row_column_count) |column| {
            refs[column] = try encoder.ref(store.stmt_rows[column].unsafeRawItemsForView());
        }
        return refs;
    }

    fn stmtRowsView(self: LirStoreImage, base_ptr: [*]align(1) u8, image_size: usize) ImageError!LirStore.StmtRowColumns {
        var columns: LirStore.StmtRowColumns = undefined;
        inline for (LirStore.stmt_row_types, 0..) |Row, column| {
            columns[column] = try guardedListFromRef(Row, "LirStore.stmt_rows", base_ptr, image_size, self.stmt_rows[column]);
        }
        return columns;
    }

    fn view(self: LirStoreImage, base_ptr: [*]align(1) u8, image_size: usize, allocator: std.mem.Allocator) ImageError!LirStore {
        if (self.runtime_contract > 1) return error.InvalidLirImage;
        return .{
            .runtime_contract = @fromBackingInt(@intCast(self.runtime_contract)),
            .cf_stmts = try guardedListFromRef(LIR.CFStmt, "LirStore.cf_stmts", base_ptr, image_size, self.cf_stmts),
            .cf_switch_branches = try guardedListFromRef(LIR.CFSwitchBranch, "LirStore.cf_switch_branches", base_ptr, image_size, self.cf_switch_branches),
            .str_match_steps = try guardedListFromRef(LIR.StrMatchStep, "LirStore.str_match_steps", base_ptr, image_size, self.str_match_steps),
            .str_match_arms = try guardedListFromRef(LIR.StrMatchArm, "LirStore.str_match_arms", base_ptr, image_size, self.str_match_arms),
            .join_points = try guardedListFromRef(LIR.JoinPoint, "LirStore.join_points", base_ptr, image_size, self.join_points),
            .locals = try guardedListFromRef(LirStore.StoredLocal, "LirStore.locals", base_ptr, image_size, self.locals),
            .local_ids = try guardedListFromRef(LIR.LocalId, "LirStore.local_ids", base_ptr, image_size, self.local_ids),
            .u64s = try guardedListFromRef(u64, "LirStore.u64s", base_ptr, image_size, self.u64s),
            .u32s = try guardedListFromRef(u32, "LirStore.u32s", base_ptr, image_size, self.u32s),
            .erased_call_arg_plans = try guardedListFromRef(LIR.ErasedCallArgsPlan, "LirStore.erased_call_arg_plans", base_ptr, image_size, self.erased_call_arg_plans),
            .stmt_rows = try self.stmtRowsView(base_ptr, image_size),
            .proc_specs = try guardedListFromRef(LIR.LirProcSpec, "LirStore.proc_specs", base_ptr, image_size, self.proc_specs),
            .strings = try self.strings.view(base_ptr, image_size),
            .string_builder = .{},
            .strings_insertable = false,
            .allocator = allocator,
            .next_synthetic_symbol = self.next_synthetic_symbol,
            .source_file_bytes = try guardedListFromRef(u8, "LirStore.source_file_bytes", base_ptr, image_size, self.source_file_bytes),
            .source_file_ends = try guardedListFromRef(u32, "LirStore.source_file_ends", base_ptr, image_size, self.source_file_ends),
            .source_file_qualified_bytes = try guardedListFromRef(u8, "LirStore.source_file_qualified_bytes", base_ptr, image_size, self.source_file_qualified_bytes),
            .source_file_qualified_ends = try guardedListFromRef(u32, "LirStore.source_file_qualified_ends", base_ptr, image_size, self.source_file_qualified_ends),
            .source_file_identities = try guardedListFromRef([32]u8, "LirStore.source_file_identities", base_ptr, image_size, self.source_file_identities),
            .lowering_module_keys = try guardedListFromRef([32]u8, "LirStore.lowering_module_keys", base_ptr, image_size, self.lowering_module_keys),
            .cf_stmt_sources = try guardedListFromRef(LirStore.StmtSourceId, "LirStore.cf_stmt_sources", base_ptr, image_size, self.cf_stmt_sources),
            .stmt_sources = try guardedListFromRef(LirStore.StmtSource, "LirStore.stmt_sources", base_ptr, image_size, self.stmt_sources),
            .cf_stmt_inline_scopes = try guardedListFromRef(LIR.InlineScopeId, "LirStore.cf_stmt_inline_scopes", base_ptr, image_size, self.cf_stmt_inline_scopes),
            .cf_stmt_origin_kinds = try guardedListFromRef(LIR.StoredOriginKind, "LirStore.cf_stmt_origin_kinds", base_ptr, image_size, self.cf_stmt_origin_kinds),
            .inline_scopes = try guardedListFromRef(LIR.InlineScope, "LirStore.inline_scopes", base_ptr, image_size, self.inline_scopes),
            .body_coordinator = null,
            .body_prefix = std.mem.zeroes(LirStore.BodyPrefix),
            .proc_locs = try guardedListFromRef(base.SourceLoc, "LirStore.proc_locs", base_ptr, image_size, self.proc_locs),
            .proc_debug_names = try guardedListFromRef(LirStore.ProcDebugName, "LirStore.proc_debug_names", base_ptr, image_size, self.proc_debug_names),
            .local_names = try guardedListFromRef(u32, "LirStore.local_names", base_ptr, image_size, self.local_names),
        };
    }
};

/// Public `StringLiteralStoreImage` declaration.
pub const StringLiteralStoreImage = extern struct {
    buffer: ArrayRef,

    fn fromStore(encoder: anytype, store: *const base.StringLiteral.Store) @TypeOf(encoder).Error!StringLiteralStoreImage {
        return .{
            .buffer = try encoder.ref(store.buffer.items.items),
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
    struct_fields: StructFieldsImage,
    struct_field_offsets: ArrayRef,
    struct_field_original_order: ArrayRef,
    struct_data: ArrayRef,
    tag_union_variants: TagUnionVariantsImage,
    tag_union_data: ArrayRef,
    recursive_layout_keys: ArrayRef,

    fn fromStore(encoder: anytype, store: *const layout_mod.Store) @TypeOf(encoder).Error!LayoutStoreImage {
        return .{
            .layouts = try encoder.ref(store.layouts.items.items),
            .resolved_list_layouts = try encoder.ref(store.resolved_list_layouts.items),
            .tuple_elems = try encoder.ref(store.tuple_elems.items.items),
            .struct_fields = try StructFieldsImage.fromStore(encoder, &store.struct_fields),
            .struct_field_offsets = try encoder.ref(store.struct_field_offsets.items.items),
            .struct_field_original_order = try encoder.ref(store.struct_field_original_order.items.items),
            .struct_data = try encoder.ref(store.struct_data.items.items),
            .tag_union_variants = try TagUnionVariantsImage.fromStore(encoder, &store.tag_union_variants),
            .tag_union_data = try encoder.ref(store.tag_union_data.items.items),
            .recursive_layout_keys = try encoder.ref(store.recursive_layout_keys.items.items),
        };
    }

    fn view(
        self: LayoutStoreImage,
        base_ptr: [*]align(1) u8,
        image_size: usize,
        target_usize: base.target.TargetUsize,
        allocator: std.mem.Allocator,
    ) ViewError!layout_mod.Store {
        var struct_fields = try self.struct_fields.view(base_ptr, image_size, allocator);
        errdefer struct_fields.deinit(allocator);
        var tag_union_variants = try self.tag_union_variants.view(base_ptr, image_size, allocator);
        errdefer tag_union_variants.deinit(allocator);
        return .{
            .allocator = allocator,
            .layouts = try safeListFromRef(layout_mod.Layout, base_ptr, image_size, self.layouts),
            .resolved_list_layouts = try arrayListFromRef(layout_mod.Idx, base_ptr, image_size, self.resolved_list_layouts),
            .tuple_elems = try safeListFromRef(layout_mod.Idx, base_ptr, image_size, self.tuple_elems),
            .struct_fields = struct_fields,
            .struct_field_offsets = try safeListFromRef(layout_mod.WidthValues(u32), base_ptr, image_size, self.struct_field_offsets),
            .struct_field_original_order = try safeListFromRef(u32, base_ptr, image_size, self.struct_field_original_order),
            .struct_data = try safeListFromRef(layout_mod.StructData, base_ptr, image_size, self.struct_data),
            .tag_union_variants = tag_union_variants,
            .tag_union_data = try safeListFromRef(layout_mod.TagUnionData, base_ptr, image_size, self.tag_union_data),
            .interned_layouts = .empty,
            .interned_key_bytes = .empty,
            .scratch_intern_key = .empty,
            .recursive_layout_keys = try safeListFromRef(layout_mod.Store.RecursiveLayoutKey, base_ptr, image_size, self.recursive_layout_keys),
            .interned_recursive_graphs = .empty,
            .recursive_unfoldings = .empty,
            .recursive_groups = .{},
            .target_usize = target_usize,
            .digest_cache = try layout_mod.DigestCache.create(allocator),
        };
    }
};

/// Portable image form of `SafeMultiList(StructField)`. Each column is a
/// normal typed array; the consumer rebuilds its target-native MultiArrayList
/// rather than interpreting the producer's private column ordering and
/// capacity layout.
pub const StructFieldsImage = extern struct {
    indices: ArrayRef,
    layouts: ArrayRef,
    is_padding: ArrayRef,

    fn fromStore(encoder: anytype, fields: *const layout_mod.StructField.SafeMultiList) @TypeOf(encoder).Error!StructFieldsImage {
        return .{
            .indices = try encoder.ref(fields.field(.index)),
            .layouts = try encoder.ref(fields.field(.layout)),
            .is_padding = try encoder.ref(fields.field(.is_padding)),
        };
    }

    fn view(
        self: StructFieldsImage,
        base_ptr: [*]align(1) u8,
        image_size: usize,
        allocator: std.mem.Allocator,
    ) ViewError!layout_mod.StructField.SafeMultiList {
        const indices = try sliceFromRef(u32, base_ptr, image_size, self.indices);
        const layouts = try sliceFromRef(layout_mod.Idx, base_ptr, image_size, self.layouts);
        const padding = try sliceFromRef(bool, base_ptr, image_size, self.is_padding);
        if (indices.len != layouts.len or indices.len != padding.len) return error.InvalidLirImage;

        var result = try layout_mod.StructField.SafeMultiList.initCapacity(allocator, indices.len);
        errdefer result.deinit(allocator);
        for (indices, layouts, padding) |index, layout_idx, is_padding| {
            _ = result.appendAssumeCapacity(.{
                .index = index,
                .layout = layout_idx,
                .is_padding = is_padding,
            });
        }
        return result;
    }
};

/// Portable image form of `SafeMultiList(TagUnionVariant)`.
pub const TagUnionVariantsImage = extern struct {
    payload_layouts: ArrayRef,

    fn fromStore(encoder: anytype, variants: *const layout_mod.TagUnionVariant.SafeMultiList) @TypeOf(encoder).Error!TagUnionVariantsImage {
        return .{
            .payload_layouts = try encoder.ref(variants.field(.payload_layout)),
        };
    }

    fn view(
        self: TagUnionVariantsImage,
        base_ptr: [*]align(1) u8,
        image_size: usize,
        allocator: std.mem.Allocator,
    ) ViewError!layout_mod.TagUnionVariant.SafeMultiList {
        const payload_layouts = try sliceFromRef(layout_mod.Idx, base_ptr, image_size, self.payload_layouts);
        var result = try layout_mod.TagUnionVariant.SafeMultiList.initCapacity(allocator, payload_layouts.len);
        errdefer result.deinit(allocator);
        for (payload_layouts) |payload_layout| {
            _ = result.appendAssumeCapacity(.{ .payload_layout = payload_layout });
        }
        return result;
    }
};

/// Make the raw bytes of persisted items a function of their values alone.
/// A persisted image writes `items` byte for byte, so each byte of
/// `T` must either belong to a declared field or be one the value itself
/// identifies, which `zeroValuePadding` canonicalizes. Any other shape, such
/// as an `extern struct` with implicit alignment bytes, is a compile error
/// here. `items` must be the persisted copy, never the store it was copied
/// from.
fn canonicalizePersistedItems(comptime T: type, items: []T) void {
    if (comptime collections.serde_validation.byteDetermination(T) == .fully_defined) return;
    for (items) |*item| collections.CompactWriter.zeroValuePadding(T, @ptrCast(item));
}

comptime {
    // The LIR image mirrors each serializable field in these three stores. When
    // a serialized field is added to or removed from a store, update the
    // matching `*Image` extern struct, its `fromStore` and `view` methods, and
    // the "LIR image round-trips every populated store field" test at the
    // bottom of this file, then update the expected field count below. A
    // same-build omission is otherwise silent, since `FORMAT_VERSION` only
    // guards cross-version mismatches. `tail_call_builder`, `proc_rewrite`,
    // `stmt_source_rows`, `rc_effect_rows` and `facts` are transient worker state, not
    // serialized, and default to null or empty in views. The layout store's `digest_cache` is a memo
    // each view starts empty, and its `commit_scratch` and
    // `scratch_field_offsets` are scratch that views start empty.
    std.debug.assert(@typeInfo(LirStore).@"struct".field_names.len == 39);
    std.debug.assert(@typeInfo(layout_mod.Store).@"struct".field_names.len == 21);
    std.debug.assert(@typeInfo(base.StringLiteral.Store).@"struct".field_names.len == 1);

    // The header is authored field by field in image storage and is part of a
    // persisted image's bytes, so every byte of it must belong to a field.
    collections.serde_validation.assertFullyDefined(Header, "LirImage.Header");
}

/// Fill the reserved LIR image header in a contiguous buffer.
///
/// `lowered` must already have been allocated from an allocator that owns
/// the buffer at `base_ptr` (the buffer must contain every pointer reachable
/// from `lowered`). This function only installs offset metadata—it does not
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
    if (lowered.static_data_values.items.len != 0) return error.InvalidLirImage;
    const encoder = Referencer{ .base_ptr = base_ptr, .image_size = image_size };
    header.* = .{
        .magic = MAGIC,
        .format_version = FORMAT_VERSION,
        .image_size = image_size,
        .root_procs = try arrayRef(base_ptr, image_size, lowered.root_procs.items),
        .platform_entrypoints = try arrayRef(base_ptr, image_size, platform_entrypoints),
        .store = try LirStoreImage.fromStore(encoder, &lowered.store),
        .layouts = try LayoutStoreImage.fromStore(encoder, &lowered.layouts),
    };
}

/// Exact copied LIR data awaiting its final image size and header.
pub const CopiedProgram = struct {
    image_capacity: usize,
    root_procs: ArrayRef,
    platform_entrypoints: ArrayRef,
    store: LirStoreImage,
    layouts: LayoutStoreImage,
    static_data: ArrayRef = .empty(),
    static_data_value_count: u32 = 0,
    static_data_pointer_bytes: u32 = 0,

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
            .static_data = self.static_data,
            .static_data_value_count = self.static_data_value_count,
            .static_data_pointer_bytes = self.static_data_pointer_bytes,
        };
    }
};

/// Retain the offsets of a program already allocated in the image buffer,
/// copying only its frozen graph. The caller supplies the final used size to
/// `fillHeader` after these graph allocations complete. The image's tables
/// are the program's own arrays, so the image is `ByteContract.mapped`:
/// canonicalizing them would modify the store.
pub fn referenceProgramWithStaticDataInBuffer(
    allocator: std.mem.Allocator,
    base_ptr: [*]align(1) const u8,
    image_capacity: usize,
    lowered: *const Program.Result,
    platform_entrypoints: []const PlatformEntrypoint,
    static_data: []const Program.StaticDataExport,
) CopyError!CopiedProgram {
    const encoder = Referencer{ .base_ptr = base_ptr, .image_size = image_capacity };
    return .{
        .image_capacity = image_capacity,
        .static_data = try StaticDataImage.copy(allocator, base_ptr, image_capacity, static_data),
        .static_data_value_count = @intCast(lowered.static_data_values.items.len),
        .static_data_pointer_bytes = @intCast(lowered.layouts.targetUsize().size()),
        .root_procs = try arrayRef(base_ptr, image_capacity, lowered.root_procs.items),
        .platform_entrypoints = try arrayRef(base_ptr, image_capacity, platform_entrypoints),
        .store = try LirStoreImage.fromStore(encoder, &lowered.store),
        .layouts = try LayoutStoreImage.fromStore(encoder, &lowered.layouts),
    };
}

/// Copy a finalized ARC-ready LIR program and its explicit frozen graph into
/// an independently allocated image buffer whose bytes follow `contract`.
/// `image_capacity` validates every copied pointer; the returned data writes a
/// header only after the caller supplies the allocator's final used size.
pub fn copyProgramWithStaticDataIntoBuffer(
    allocator: std.mem.Allocator,
    base_ptr: [*]align(1) const u8,
    image_capacity: usize,
    contract: ByteContract,
    lowered: *const Program.Result,
    platform_entrypoints: []const PlatformEntrypoint,
    static_data: []const Program.StaticDataExport,
) CopyError!CopiedProgram {
    const encoder = Copier{ .allocator = allocator, .base_ptr = base_ptr, .image_capacity = image_capacity, .contract = contract };
    return .{
        .image_capacity = image_capacity,
        .static_data = try StaticDataImage.copy(allocator, base_ptr, image_capacity, static_data),
        .static_data_value_count = @intCast(lowered.static_data_values.items.len),
        .static_data_pointer_bytes = @intCast(lowered.layouts.targetUsize().size()),
        .root_procs = try copyArrayRef(allocator, base_ptr, image_capacity, contract, lowered.root_procs.items),
        .platform_entrypoints = try copyArrayRef(allocator, base_ptr, image_capacity, contract, platform_entrypoints),
        .store = try LirStoreImage.fromStore(encoder, &lowered.store),
        .layouts = try LayoutStoreImage.fromStore(encoder, &lowered.layouts),
    };
}

/// View an ARC-inserted LIR program in place from a mapped buffer.
///
/// The buffer is treated as read-only by the view—`LirStore` and
/// `layout_mod.Store` are constructed with slices that the interpreter
/// reads but never mutates. Accepting `const` here lets embedders that
/// hold the buffer behind a `const` pointer (e.g. a `FixedBufferAllocator`
/// backed by `gpa.alignedAlloc` whose owning slice is `const`) pass it
/// directly without a manual `@constCast`.
pub fn viewMappedImage(header: *const Header, base_ptr: [*]align(1) const u8, mapped_size: usize, target_usize: base.target.TargetUsize) ViewError!ProgramView {
    return viewMappedImageWithAllocator(header, base_ptr, mapped_size, target_usize, base.defaultGpa());
}

/// View an ARC-inserted LIR program in place from a mapped buffer using the
/// provided allocator for any scratch data owned by reconstructed stores.
///
/// The image contents (LIR op stream and layout store) are pointer-width
/// independent, so the caller supplies the width to resolve layout sizes,
/// offsets, and alignments for. The same image bytes can be viewed for either
/// width—e.g. a cross-width cache reused by both a native interpreter and a
/// 32-bit codegen backend.
pub fn viewMappedImageWithAllocator(
    header: *const Header,
    base_ptr: [*]align(1) const u8,
    mapped_size: usize,
    target_usize: base.target.TargetUsize,
    allocator: std.mem.Allocator,
) ViewError!ProgramView {
    if (mapped_size < @sizeOf(Header)) return error.InvalidLirImage;

    if (header.magic != MAGIC) return error.InvalidLirImage;
    if (header.format_version != FORMAT_VERSION) return error.UnsupportedLirImageVersion;
    if (header.image_size > mapped_size) return error.InvalidLirImage;
    if (header.static_data.len != 0 and header.static_data_pointer_bytes != target_usize.size()) return error.InvalidLirImage;

    // The view path constructs mutable container types (LirStore, Store)
    // whose slice fields are not const, even though the interpreter only
    // reads them. Cast once at the boundary so callers don't have to.
    const mutable_base: [*]align(1) u8 = @constCast(base_ptr);
    var layouts = try header.layouts.view(mutable_base, @intCast(header.image_size), target_usize, allocator);
    errdefer deinitViewedLayouts(&layouts, allocator);
    const static_data = try StaticDataImage.view(allocator, mutable_base, @intCast(header.image_size), header.static_data, header.static_data_value_count, @intCast(header.store.proc_specs.len), layouts.layoutCount(), target_usize.size());
    errdefer StaticDataImage.deinit(allocator, static_data);
    return .{
        .static_data = static_data,
        .static_data_value_count = header.static_data_value_count,
        .store = try header.store.view(mutable_base, @intCast(header.image_size), allocator),
        .layouts = layouts,
        .root_procs = try sliceFromRef(LIR.LirProcSpecId, mutable_base, @intCast(header.image_size), header.root_procs),
        .platform_entrypoints = try sliceFromRef(PlatformEntrypoint, mutable_base, @intCast(header.image_size), header.platform_entrypoints),
        .target_usize = target_usize,
        .scratch_allocator = allocator,
    };
}

fn deinitViewedLayouts(layouts: *layout_mod.Store, allocator: std.mem.Allocator) void {
    layouts.struct_fields.deinit(allocator);
    layouts.tag_union_variants.deinit(allocator);
    layouts.interned_layouts.deinit(allocator);
    layouts.interned_key_bytes.deinit(allocator);
    layouts.scratch_field_offsets.deinit(allocator);
    layouts.commit_scratch.promote(allocator).deinit();
    layouts.digest_cache.destroy(allocator);
}

/// Encodes arrays that are already inside the image as checked relative
/// references.
pub const Referencer = struct {
    base_ptr: [*]align(1) const u8,
    image_size: usize,

    pub const Error = ImageError;

    fn ref(self: Referencer, slice: anytype) ImageError!ArrayRef {
        return arrayRef(self.base_ptr, self.image_size, slice);
    }
};

/// Copies arrays into the image allocator and encodes the copies.
pub const Copier = struct {
    allocator: std.mem.Allocator,
    base_ptr: [*]align(1) const u8,
    image_capacity: usize,
    contract: ByteContract,

    pub const Error = CopyError;

    fn ref(self: Copier, source: anytype) CopyError!ArrayRef {
        return copyArrayRef(self.allocator, self.base_ptr, self.image_capacity, self.contract, source);
    }
};

/// Encode a slice already inside the image as a checked relative reference.
pub fn arrayRef(base_ptr: [*]align(1) const u8, image_size: usize, slice: anytype) ImageError!ArrayRef {
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

/// Copy a slice with the image allocator and encode its relative reference.
///
/// The item type is classified at compile time under either contract, so
/// a type with undefined bytes that nothing identifies cannot enter an image.
/// A `.mapped` copy is the verbatim duplicate and pays for nothing else; a
/// `.persisted` copy additionally canonicalizes each scrubbable item in
/// the duplicate.
pub fn copyArrayRef(
    allocator: std.mem.Allocator,
    base_ptr: [*]align(1) const u8,
    image_capacity: usize,
    contract: ByteContract,
    source: anytype,
) CopyError!ArrayRef {
    const T = std.meta.Child(@TypeOf(source));
    _ = comptime collections.serde_validation.byteDetermination(T);
    if (source.len == 0) return ArrayRef.empty();
    const copied = try allocator.dupe(T, source);
    switch (contract) {
        .mapped => {},
        .persisted => canonicalizePersistedItems(T, copied),
    }
    return try arrayRef(base_ptr, image_capacity, copied);
}

/// Validate and resolve a typed slice reference within the mapped image.
pub fn sliceFromRef(comptime T: type, base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError![]T {
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
        .pointer_stability = .{},
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
        .items = list,
    };
}

fn stringLiteralBufferFromRef(base_ptr: [*]align(1) u8, image_size: usize, ref: ArrayRef) ImageError!base.StringLiteral.Store.Buffer {
    const len, const capacity = try checkByteListRef(image_size, ref);
    if (capacity == 0) return .{};

    const ptr: [*]u8 = @ptrCast(base_ptr + try checkedOffset(ref));
    return base.StringLiteral.Store.Buffer.fromMappedSlice(ptr[0..len], capacity);
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

test "image header and frozen graph rows declare every byte they occupy" {
    try std.testing.expect(collections.serde_validation.isFullyDefined(Header));
    try std.testing.expect(collections.serde_validation.isFullyDefined(StaticDataImage.Export));
    try std.testing.expect(collections.serde_validation.isFullyDefined(StaticDataImage.Relocation));
}

test "persisted image bytes do not depend on what store memory held before" {
    const gpa = std.testing.allocator;
    var images: [2][]align(16) u8 = undefined;
    var built: usize = 0;
    defer for (images[0..built]) |image| gpa.free(image);

    for ([_]u8{ 0x00, 0xff }) |stale| {
        var lowered = try Program.Result.init(gpa, .u64);
        defer lowered.deinit();
        const store = &lowered.store;

        // Write the same values over storage that held different bytes, so
        // any byte a value does not define keeps what was there: the payload
        // of a null optional, the bytes of a tagged union past its active
        // variant, and the gaps between a struct's fields.
        const local = try store.addLocal(.{ .layout_idx = .u64 });
        const local_slot = store.getLocalPtr(local);
        @memset(std.mem.asBytes(local_slot), stale);
        local_slot.* = .{ .layout_idx = .u64 };

        const ret = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
        const stmt_slot = store.getCFStmtPtr(ret);
        @memset(std.mem.asBytes(stmt_slot), stale);
        stmt_slot.* = .{ .ret = .{ .value = local } };

        const name = store.freshSyntheticSymbol();
        const proc = try store.addProcSpec(.{
            .name = name,
            .identity = LIR.ProcIdentity.forTest(1),
            .args = .empty(),
            .body = ret,
            .ret_layout = .u64,
        }, .none);
        const proc_slot = store.getProcSpecPtr(proc);
        @memset(std.mem.asBytes(proc_slot), stale);
        proc_slot.* = .{
            .name = name,
            .identity = LIR.ProcIdentity.forTest(1),
            .args = .empty(),
            .body = ret,
            .ret_layout = .u64,
        };
        try lowered.root_procs.append(gpa, proc);

        const source_stmts = try gpa.dupe(u8, std.mem.sliceAsBytes(store.cf_stmts.unsafeRawItemsForView()));
        defer gpa.free(source_stmts);

        // A persisted image's producer supplies zero-filled storage.
        const buffer = try gpa.alignedAlloc(u8, .@"16", 1 << 16);
        defer gpa.free(buffer);
        @memset(buffer, 0);
        var fba = std.heap.FixedBufferAllocator.init(buffer);
        const header = try fba.allocator().create(Header);
        const copied = try copyProgramWithStaticDataIntoBuffer(fba.allocator(), buffer.ptr, buffer.len, .persisted, &lowered, &.{}, &.{});
        try copied.fillHeader(header, fba.end_index);

        // Canonicalization happens in the image's copy; the store keeps its bytes.
        try std.testing.expectEqualSlices(u8, source_stmts, std.mem.sliceAsBytes(store.cf_stmts.unsafeRawItemsForView()));

        // The canonical image still holds the values the store holds.
        var view = try viewMappedImageWithAllocator(header, buffer.ptr, fba.end_index, .u64, gpa);
        defer view.deinit();
        try std.testing.expectEqual(local, view.store.getCFStmtImmediate(ret).ret.value);
        try std.testing.expectEqual(layout_mod.Idx.u64, view.store.getLocal(local).layout_idx);
        try std.testing.expectEqual(@as(?LIR.CFStmtId, ret), view.store.getProcSpecImmediate(proc).body);
        try std.testing.expectEqual(name, view.store.getProcSpecImmediate(proc).name);
        try std.testing.expectEqual(proc, view.root_procs[0]);

        images[built] = try gpa.alignedAlloc(u8, .@"16", fba.end_index);
        @memcpy(images[built], buffer[0..fba.end_index]);
        built += 1;
    }

    try std.testing.expectEqualSlices(u8, images[0], images[1]);
}

test "LIR image declarations are referenced" {
    std.testing.refAllDecls(@This());
}

/// The 25 `LirStore` array-backed lists serialized as `ArrayRef`s, in the order
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
    "source_file_qualified_bytes",
    "source_file_qualified_ends",
    "source_file_identities",
    "lowering_module_keys",
    "cf_stmt_sources",
    "stmt_sources",
    "cf_stmt_inline_scopes",
    "cf_stmt_origin_kinds",
    "inline_scopes",
    "proc_locs",
    "proc_debug_names",
    "local_names",
};

test "LIR image round-trips ordered procedure rewrites with relocated suffixes" {
    const gpa = std.testing.allocator;
    var lowered = try Program.Result.init(gpa, .u64);
    defer lowered.deinit();
    const store = &lowered.store;
    const pointer_layout = try lowered.layouts.insertPtr(.str);
    const original = try store.addLocal(.{ .layout_idx = pointer_layout });
    _ = try store.insertString("frozen prefix");
    var roots: [2]LIR.CFStmtId = undefined;
    var procs: [2]LIR.LirProcSpecId = undefined;
    for (&roots, &procs) |*root, *proc| {
        root.* = try store.addCFStmt(.{ .ret = .{ .value = original } }, .test_fixture);
        proc.* = try store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(store.procSpecCount())),
            .name = store.freshSyntheticSymbol(),
            .args = .empty(),
            .body = root.*,
            .ret_layout = pointer_layout,
        }, .none);
    }
    const prefix = store.captureBodyPrefix();
    var first = try store.cloneForProcRewrite(gpa, procs[0]);
    defer first.deinit();
    var second = try store.cloneForProcRewrite(gpa, procs[1]);
    defer second.deinit();
    // Both shards start with the same suffix identities. The second commit
    // must relocate them before the image can forget all worker overlay state.
    const shard_loc: base.SourceLoc = .{ .file = 1, .line = 2, .column = 3 };
    for ([_]*LirStore{ &first, &second }, roots, [_][]const u8{ "first", "second" }) |worker, root, name| {
        const local = try worker.addLocal(.{ .layout_idx = pointer_layout });
        try worker.setLocalName(local, name);
        const string = try worker.insertString(name);
        const inline_scope = try worker.addInlineScope(.{
            .source_identity = store.getProcSpecImmediate(procs[0]).identity,
            .copies_site_code = false,
            .source_name = string,
            .source_loc = shard_loc,
            .call_site = .{ .file = 1, .line = 4, .column = 5 },
            .parent = .none,
        });
        const ret = try worker.addCFStmt(.{ .ret = .{ .value = local } }, .{
            .loc = shard_loc,
            .region = base.Region.zero(),
            .inline_scope = inline_scope,
            .kind = .source,
        });
        worker.getCFStmtPtr(root).* = .{ .assign_ref = .{
            .target = local,
            .op = .local(original),
            .next = ret,
        } };
    }
    try store.commitProcRewriteWithJoinRelocation(&first, null);
    try store.commitProcRewriteWithJoinRelocation(&second, null);
    try std.testing.expectEqual(prefix.locals + 2, store.captureBodyPrefix().locals);
    try std.testing.expectEqual(prefix.cf_stmts + 2, store.captureBodyPrefix().cf_stmts);
    for (roots, 0..) |root, index| {
        const assign = store.getCFStmtImmediate(root).assign_ref;
        try std.testing.expectEqual(prefix.locals + index, @backingInt(assign.target));
        try std.testing.expectEqual(prefix.cf_stmts + index, @backingInt(assign.next));
        try std.testing.expectEqual(assign.target, store.getCFStmtImmediate(assign.next).ret.value);
    }

    const buffer = try gpa.alignedAlloc(u8, .@"16", 1 << 20);
    defer gpa.free(buffer);
    var fba = std.heap.FixedBufferAllocator.init(buffer);
    const header = try fba.allocator().create(Header);
    const copied = try copyProgramWithStaticDataIntoBuffer(fba.allocator(), buffer.ptr, buffer.len, .mapped, &lowered, &.{}, &.{});
    try copied.fillHeader(header, fba.end_index);
    var view = try viewMappedImageWithAllocator(header, buffer.ptr, buffer.len, .u64, gpa);
    defer view.deinit();
    try std.testing.expect(view.store.proc_rewrite == null);
    try std.testing.expect(view.store.body_coordinator == null);
    try std.testing.expectEqual(std.mem.zeroes(LirStore.BodyPrefix), view.store.body_prefix);
    inline for (serialized_guarded_fields) |field| {
        const source = @field(store, field).unsafeRawItemsForView();
        const mapped = @field(view.store, field).unsafeRawItemsForView();
        try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(source), std.mem.sliceAsBytes(mapped));
    }
    try std.testing.expectEqualSlices(u8, store.strings.buffer.items.items, view.store.strings.buffer.items.items);
    try std.testing.expectEqual(store.next_synthetic_symbol, view.store.next_synthetic_symbol);
    try std.testing.expectEqual(lowered.layouts.layoutCount(), view.layouts.layoutCount());
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(lowered.layouts.layouts.items.items), std.mem.sliceAsBytes(view.layouts.layouts.items.items));
    for (roots, [_][]const u8{ "first", "second" }) |root, name| {
        const assign = view.store.getCFStmtImmediate(root).assign_ref;
        try std.testing.expectEqual(pointer_layout, view.store.getLocal(assign.target).layout_idx);
        try std.testing.expectEqualDeep(lowered.layouts.getLayout(pointer_layout), view.layouts.getLayout(pointer_layout));
        const scope = view.store.inlineScope(view.store.stmtInlineScope(assign.next));
        try std.testing.expectEqualStrings(name, view.store.getString(scope.source_name));
        try std.testing.expectEqual(shard_loc, view.store.stmtLoc(assign.next));
    }
}

test "LIR image copies and round-trips every populated store field" {
    const gpa = std.testing.allocator;

    // Build the compiler result outside the image buffer so this test proves
    // copying transfers every mapped field instead of retaining source
    // pointers.
    var source_arena = base.SingleThreadArena.init(gpa);
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
            return FieldT.fromArrayList(.{ .items = slice, .capacity = count, .pointer_stability = .{} });
        }
        /// Build a populated `SafeList(T)` backed by the fixed buffer.
        fn safeList(comptime T: type, alloc: std.mem.Allocator, count: usize, seed: u8) std.mem.Allocator.Error!collections.SafeList(T) {
            const slice = try distinct(T, alloc, count, seed);
            return .{ .items = .{ .items = slice, .capacity = count, .pointer_stability = .{} } };
        }
        /// A per-field-and-index distinctive *value* of `T`. Filling raw bytes
        /// instead would write bit patterns no value of the type can hold—an
        /// `Idx` is 28 bits in a four-byte slot and a `bool` is one of two
        /// bytes—and a copy that moves values, as the MultiArrayList columns
        /// do, is free not to reproduce them.
        fn distinctValue(comptime T: type, ordinal: usize) T {
            if (T == bool) return ordinal & 1 == 1;
            const info = @typeInfo(T);
            if (info == .int) return @truncate(ordinal);
            if (info == .@"enum") return @fromBackingInt(@intCast(@as(info.@"enum".tag_type, @truncate(ordinal))));
            @compileError("distinctValue: unhandled field type " ++ @typeName(T));
        }
        /// Build a populated `SafeMultiList(T)` backed by the fixed buffer,
        /// giving every column its own value sequence so a dropped or swapped
        /// field is detectable after view.
        fn multiList(comptime T: type, alloc: std.mem.Allocator, count: usize, seed: u8) std.mem.Allocator.Error!collections.SafeMultiList(T) {
            var mal: std.MultiArrayList(T) = .{};
            try mal.resize(alloc, count);
            const slice = mal.slice();
            inline for (@typeInfo(T).@"struct".field_names, 0..) |field_name, field_index| {
                const column = slice.items(@field(std.MultiArrayList(T).Field, field_name));
                for (column, 0..) |*value, i| {
                    value.* = distinctValue(@FieldType(T, field_name), seed + field_index * 64 + i);
                }
            }
            return .{ .items = mal };
        }
        /// Assert two byte spans are equal and non-empty.
        fn expectBytesEq(a: []const u8, b: []const u8) error{ TestExpectedEqual, TestUnexpectedResult }!void {
            try std.testing.expect(a.len > 0);
            try std.testing.expectEqualSlices(u8, a, b);
        }
        /// Assert two spans hold equal values and are non-empty. Used for a
        /// column whose element type has padding bits: `Idx` is 28 bits in a
        /// four-byte slot, so the bytes the fill wrote above bit 27 are not
        /// part of any value and a copy is free to drop them.
        fn expectValuesEq(comptime T: type, a: []const T, b: []const T) error{ TestExpectedEqual, TestUnexpectedResult }!void {
            try std.testing.expect(a.len > 0);
            try std.testing.expectEqualSlices(T, a, b);
        }
    };

    // A LirStore with every serialized list populated distinctively.
    var store = LirStore.init(source_allocator);
    inline for (serialized_guarded_fields, 0..) |fname, i| {
        @field(store, fname) = try h.guarded(@FieldType(LirStore, fname), source_allocator, 2 + i, @intCast(0x20 + i));
    }
    inline for (0..LirStore.stmt_row_column_count) |column| {
        store.stmt_rows[column] = try h.guarded(@TypeOf(store.stmt_rows[column]), source_allocator, 2 + column, @intCast(0x60 + column));
    }
    store.next_synthetic_symbol = 0x0123_4567_89ab_cdef;
    store.strings = .{ .buffer = .{ .items = .{ .items = try h.distinct(u8, source_allocator, 24, 0x90), .capacity = 24, .pointer_stability = .{} } } };

    // A layout Store with every serialized list populated distinctively. Only
    // the ten array-backed fields are serialized; the interning caches are not
    // read by `fromStore`, so they are left undefined here.
    var layouts = layout_mod.Store{
        .allocator = gpa,
        .layouts = try h.safeList(layout_mod.Layout, source_allocator, 3, 0x40),
        .resolved_list_layouts = .{ .items = try h.distinct(layout_mod.Idx, source_allocator, 4, 0x50), .capacity = 4, .pointer_stability = .{} },
        .tuple_elems = try h.safeList(layout_mod.Idx, source_allocator, 5, 0x60),
        .struct_fields = try h.multiList(layout_mod.StructField, source_allocator, 6, 0x70),
        .struct_field_offsets = try h.safeList(layout_mod.WidthValues(u32), source_allocator, 6, 0x74),
        .struct_field_original_order = try h.safeList(u32, source_allocator, 6, 0x78),
        .struct_data = try h.safeList(layout_mod.StructData, source_allocator, 7, 0x80),
        .tag_union_variants = try h.multiList(layout_mod.TagUnionVariant, source_allocator, 8, 0x88),
        .tag_union_data = try h.safeList(layout_mod.TagUnionData, source_allocator, 9, 0xa0),
        .interned_layouts = undefined,
        .interned_key_bytes = undefined,
        .scratch_intern_key = undefined,
        .recursive_layout_keys = try h.safeList(layout_mod.Store.RecursiveLayoutKey, source_allocator, 2, 0xa8),
        .interned_recursive_graphs = undefined,
        .recursive_unfoldings = undefined,
        .recursive_groups = undefined,
        .target_usize = target_usize,
        .digest_cache = undefined,
    };

    const root_procs = try h.distinct(LIR.LirProcSpecId, source_allocator, 3, 0xb0);
    const entrypoints = try h.distinct(PlatformEntrypoint, source_allocator, 2, 0xc0);

    const base_ptr = buffer.ptr;
    const header = try fba.create(Header);
    var lowered = try Program.Result.init(source_allocator, target_usize);
    lowered.store = store;
    lowered.layouts = layouts;
    lowered.root_procs = .{ .items = root_procs, .capacity = root_procs.len, .pointer_stability = .{} };
    const copied = try copyProgramWithStaticDataIntoBuffer(fba, base_ptr, buffer.len, .mapped, &lowered, entrypoints, &.{});
    try copied.fillHeader(header, fba_state.end_index);

    // View back over the same buffer.
    var view = try viewMappedImageWithAllocator(header, base_ptr, buffer.len, target_usize, gpa);
    defer view.deinit();

    // Every serialized guarded list must round-trip byte-for-byte. A field
    // omitted from `fromStore`/`view` would read back as empty and fail here.
    inline for (serialized_guarded_fields) |fname| {
        const a = @field(store, fname).unsafeRawItemsForView();
        const b = @field(view.store, fname).unsafeRawItemsForView();
        try std.testing.expectEqual(a.len, b.len);
        try h.expectBytesEq(std.mem.sliceAsBytes(a), std.mem.sliceAsBytes(b));
    }
    inline for (0..LirStore.stmt_row_column_count) |column| {
        const a = store.stmt_rows[column].unsafeRawItemsForView();
        const b = view.store.stmt_rows[column].unsafeRawItemsForView();
        try std.testing.expectEqual(a.len, b.len);
        try h.expectBytesEq(std.mem.sliceAsBytes(a), std.mem.sliceAsBytes(b));
    }

    // Scalar and sub-image fields.
    try std.testing.expectEqual(@as(u64, 0x0123_4567_89ab_cdef), view.store.next_synthetic_symbol);
    try h.expectBytesEq(store.strings.buffer.items.items, view.store.strings.buffer.items.items);

    try std.testing.expectEqual(@as(?*const LirStore, null), view.store.body_coordinator);
    try std.testing.expect(view.store.proc_rewrite == null);
    try std.testing.expectEqual(std.mem.zeroes(LirStore.BodyPrefix), view.store.body_prefix);
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
        std.mem.sliceAsBytes(layouts.struct_field_offsets.items.items),
        std.mem.sliceAsBytes(view.layouts.struct_field_offsets.items.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.struct_field_original_order.items.items),
        std.mem.sliceAsBytes(view.layouts.struct_field_original_order.items.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.tag_union_data.items.items),
        std.mem.sliceAsBytes(view.layouts.tag_union_data.items.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.recursive_layout_keys.items.items),
        std.mem.sliceAsBytes(view.layouts.recursive_layout_keys.items.items),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.struct_fields.field(.index)),
        std.mem.sliceAsBytes(view.layouts.struct_fields.field(.index)),
    );
    try h.expectValuesEq(
        layout_mod.Idx,
        layouts.struct_fields.field(.layout),
        view.layouts.struct_fields.field(.layout),
    );
    try h.expectBytesEq(
        std.mem.sliceAsBytes(layouts.struct_fields.field(.is_padding)),
        std.mem.sliceAsBytes(view.layouts.struct_fields.field(.is_padding)),
    );
    try h.expectValuesEq(
        layout_mod.Idx,
        layouts.tag_union_variants.field(.payload_layout),
        view.layouts.tag_union_variants.field(.payload_layout),
    );
    try std.testing.expectEqual(target_usize, view.layouts.target_usize);
    try std.testing.expectEqual(target_usize, view.target_usize);

    // Header-level array refs.
    try std.testing.expectEqual(@as(usize, 3), view.root_procs.len);
    try std.testing.expectEqual(@as(usize, 2), view.platform_entrypoints.len);
    try h.expectBytesEq(std.mem.sliceAsBytes(root_procs), std.mem.sliceAsBytes(view.root_procs));
    try h.expectBytesEq(std.mem.sliceAsBytes(entrypoints), std.mem.sliceAsBytes(view.platform_entrypoints));
}

test "mapped frozen graph preserves explicit data callable and helper relocations" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const value_slot: LIR.StaticDataId = @fromBackingInt(@intCast(program.static_data_values.items.len));
    try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = .u64 });
    const worker = try program.store.addProcSpec(.{ .name = program.store.freshSyntheticSymbol(), .identity = LIR.ProcIdentity.forTest(2), .args = .empty(), .body = null, .ret_layout = .zst }, .none);
    const memory = try allocator.alignedAlloc(u8, .@"16", 16384);
    defer allocator.free(memory);
    var fixed = std.heap.FixedBufferAllocator.init(memory);
    var bytes = @as([32]u8, @splat(0));
    const graph = [_]Program.StaticDataExport{
        .{ .symbol_name = "value", .value_id = value_slot, .bytes = &bytes, .alignment = 8, .relocations = &.{
            .{ .offset = 0, .target_symbol_name = "backing", .target = .{ .data_symbol = @fromBackingInt(@intCast(1)) }, .addend = 2 },
            .{ .offset = 8, .target_symbol_name = "worker", .kind = .function_pointer, .procedure = worker, .callable_capture_offset = 24 },
            .{ .offset = 16, .target_symbol_name = "drop", .kind = .function_pointer, .rc_helper = .{ .op = .decref, .layout_idx = .str } },
        } },
        .{ .symbol_name = "backing", .bytes = "frozen data", .alignment = 1 },
    };
    const copied = try StaticDataImage.copy(fixed.allocator(), memory.ptr, memory.len, &graph);
    bytes[0] = 99;
    const viewed = try StaticDataImage.view(allocator, memory.ptr, fixed.end_index, copied, @intCast(program.static_data_values.items.len), program.store.procSpecCount(), program.layouts.layoutCount(), 8);
    defer StaticDataImage.deinit(allocator, viewed);
    try std.testing.expectEqual(@as(u8, 0), viewed[0].bytes[0]);
    try std.testing.expectEqualStrings("frozen data", viewed[1].bytes);
    try std.testing.expectEqual(@as(Program.StaticDataSymbolId, @fromBackingInt(@intCast(1))), viewed[0].relocations[0].target.data_symbol);
    try std.testing.expectEqual(@as(i64, 2), viewed[0].relocations[0].addend);
    try std.testing.expectEqual(@as(?LIR.LirProcSpecId, worker), viewed[0].relocations[1].procedure);
    try std.testing.expectEqual(@as(?u32, 24), viewed[0].relocations[1].callable_capture_offset);
    try std.testing.expectEqual(layout_mod.RcOp.decref, viewed[0].relocations[2].rc_helper.?.op);
    try std.testing.expectEqual(layout_mod.Idx.str, viewed[0].relocations[2].rc_helper.?.layout_idx);
    const rows = try sliceFromRef(StaticDataImage.Export, memory.ptr, fixed.end_index, copied);
    const relocations = try sliceFromRef(StaticDataImage.Relocation, memory.ptr, fixed.end_index, rows[0].relocations);
    relocations[2].rc_layout = @intCast(program.layouts.layoutCount());
    try std.testing.expectError(error.InvalidLirImage, StaticDataImage.view(allocator, memory.ptr, fixed.end_index, copied, @intCast(program.static_data_values.items.len), program.store.procSpecCount(), program.layouts.layoutCount(), 8));
    relocations[2].rc_layout = @backingInt(layout_mod.Idx.str);
    rows[0].value_id = 1;
    try std.testing.expectError(error.InvalidLirImage, StaticDataImage.view(allocator, memory.ptr, fixed.end_index, copied, @intCast(program.static_data_values.items.len), program.store.procSpecCount(), program.layouts.layoutCount(), 8));
}

test "mapped frozen graph rejects a consumer pointer width mismatch" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const value_slot: LIR.StaticDataId = @fromBackingInt(@intCast(program.static_data_values.items.len));
    try program.static_data_values.append(allocator, .{ .initializer = null, .layout_idx = .u8 });
    const memory = try allocator.alignedAlloc(u8, .@"16", 65536);
    defer allocator.free(memory);
    var fixed = std.heap.FixedBufferAllocator.init(memory);
    const header = try fixed.allocator().create(Header);
    const copied = try copyProgramWithStaticDataIntoBuffer(fixed.allocator(), memory.ptr, memory.len, .mapped, &program, &.{}, &.{.{ .symbol_name = "value", .value_id = value_slot, .bytes = &.{42}, .alignment = 1 }});
    try copied.fillHeader(header, fixed.end_index);
    try std.testing.expectError(error.InvalidLirImage, viewMappedImageWithAllocator(header, memory.ptr, fixed.end_index, .u32, allocator));
    var view = try viewMappedImageWithAllocator(header, memory.ptr, fixed.end_index, .u64, allocator);
    defer view.deinit();
    try std.testing.expectEqual(@as(u8, 42), view.static_data[0].bytes[0]);
    try std.testing.expectEqual(@as(u32, 1), view.static_data_value_count);
}

test "in-place frozen image retains existing LIR arrays" {
    const allocator = std.testing.allocator;
    const memory = try allocator.alignedAlloc(u8, .@"16", 65536);
    defer allocator.free(memory);
    var fixed = std.heap.FixedBufferAllocator.init(memory);
    const image_allocator = fixed.allocator();
    const header = try image_allocator.create(Header);
    var program = try Program.Result.init(image_allocator, .native);
    defer program.deinit();
    const value_slot: LIR.StaticDataId = @fromBackingInt(@intCast(program.static_data_values.items.len));
    try program.static_data_values.append(image_allocator, .{ .initializer = null, .layout_idx = .u64 });
    const local = try program.store.addLocal(.{ .layout_idx = .u64 });
    const ret = try program.store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = value_slot } }, .next = ret } }, .test_fixture);
    const proc = try program.store.addProcSpec(.{ .name = .fromRaw(1), .identity = LIR.ProcIdentity.forTest(1), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{local}), .body = body, .ret_layout = .u64 }, .none);
    try program.root_procs.append(image_allocator, proc);
    var value: [8]u8 = undefined;
    std.mem.writeInt(u64, &value, 42, .little);
    const prepared = try referenceProgramWithStaticDataInBuffer(image_allocator, memory.ptr, memory.len, &program, &.{}, &.{.{ .symbol_name = "value", .value_id = value_slot, .bytes = &value, .alignment = 8 }});
    try prepared.fillHeader(header, fixed.end_index);
    var view = try viewMappedImageWithAllocator(header, memory.ptr, fixed.end_index, .native, allocator);
    defer view.deinit();
    try std.testing.expectEqual(@as(u64, fixed.end_index), header.image_size);
    try std.testing.expectEqual(@intFromPtr(program.store.getProcSpecs().ptr), @intFromPtr(view.store.getProcSpecs().ptr));
    try std.testing.expectEqual(@as(u32, 1), view.static_data_value_count);
    try std.testing.expectEqualSlices(u8, &value, view.static_data[0].bytes);
    try std.testing.expect(@intFromPtr(view.static_data[0].bytes.ptr) >= @intFromPtr(memory.ptr));
    try std.testing.expect(@intFromPtr(view.static_data[0].bytes.ptr) + value.len <= @intFromPtr(memory.ptr) + fixed.end_index);
}

// repro for https://github.com/roc-lang/roc/issues/12480
// A viewed image's layout store must compute the same content digest for a
// recursive layout as the store the image was copied from.
test "a viewed LIR image digests a recursive layout" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();

    // `Node := [Nil, Cons(Box(Node))]`, as in layout/digest.zig's tests.
    var graph = layout_mod.Graph{};
    defer graph.deinit(allocator);
    const union_node = try graph.reserveNode(allocator);
    const box_node = try graph.reserveNode(allocator);
    graph.setNode(box_node, .{ .box = .{ .local = union_node } });
    graph.setNode(union_node, .{ .tag_union = try graph.appendRefs(allocator, &[_]layout_mod.GraphRef{ .{ .canonical = .zst }, .{ .local = box_node } }) });
    var commit = try program.layouts.commitGraph(&graph, .{ .local = union_node });
    defer commit.deinit(allocator);
    const root = commit.root_idx;
    const expected = try program.layouts.contentDigest(root);

    const memory = try allocator.alignedAlloc(u8, .@"16", 1 << 16);
    defer allocator.free(memory);
    var fixed = std.heap.FixedBufferAllocator.init(memory);
    const header = try fixed.allocator().create(Header);
    const copied = try copyProgramWithStaticDataIntoBuffer(fixed.allocator(), memory.ptr, memory.len, .mapped, &program, &.{}, &.{});
    try copied.fillHeader(header, fixed.end_index);
    var view = try viewMappedImageWithAllocator(header, memory.ptr, fixed.end_index, .u64, allocator);
    defer view.deinit();

    try std.testing.expectEqualSlices(u8, &expected, &(try view.layouts.contentDigest(root)));
}

test "LIR image preserves the specialized runtime contract and rejects incompatible artifacts" {
    const allocator = std.testing.allocator;
    const buffer = try allocator.alignedAlloc(u8, .@"16", 1 << 20);
    defer allocator.free(buffer);
    var storage = std.heap.FixedBufferAllocator.init(buffer);
    const header = try storage.allocator().create(Header);
    var program = try Program.Result.init(storage.allocator(), .u64);
    try fillHeaderInBuffer(header, buffer.ptr, buffer.len, &program, &.{});
    var view = try viewMappedImageWithAllocator(header, buffer.ptr, buffer.len, .u64, allocator);
    defer view.deinit();
    try std.testing.expectEqual(LIR.RuntimeContract.specialized, view.store.runtime_contract);
    header.format_version = FORMAT_VERSION - 1;
    try std.testing.expectError(error.UnsupportedLirImageVersion, viewMappedImageWithAllocator(header, buffer.ptr, buffer.len, .u64, allocator));
    header.format_version = FORMAT_VERSION;
    header.store.runtime_contract = 2;
    try std.testing.expectError(error.InvalidLirImage, viewMappedImageWithAllocator(header, buffer.ptr, buffer.len, .u64, allocator));
}
