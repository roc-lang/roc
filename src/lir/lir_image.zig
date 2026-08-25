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
/// v11: LIR images carry reachable boxy descriptor/dictionary tables.
/// v12: LIR RC statements carry explicit concrete-or-boxy helper metadata.
/// v13: LIR images carry boxy adapter plan tables.
/// v14: LocalSpan lengths are u32 for large proc frame-local spans.
/// v15: SafeMultiList layout tables use portable typed column arrays.
/// v16: procedure specs carry explicit Boxy return descriptor sources.
/// v17: dictionary method adapters carry exact call descriptor spans.
/// v18: erased calls carry exact keyed argument descriptor metadata.
/// v19: erased calls and workers carry ordered runtime argument layouts.
/// v20: statements carry virtual inline-scope ids and the image stores the
///      corresponding source-procedure/call-site graph.
/// v21: erased calls and procedures carry explicit packed-argument plans.
/// v22: combines the independent v20 and v21 image changes.
/// v23: Boxy type descriptors identify field-presence slots explicitly.
/// v24: integer arithmetic uses explicit behavior-family operations.
pub const FORMAT_VERSION: u32 = 24;

/// Public `ImageError` declaration.
pub const ImageError = error{
    InvalidLirImage,
    UnsupportedLirImageVersion,
};

/// Errors produced while reconstructing a mapped image view.
pub const ViewError = ImageError || std.mem.Allocator.Error;

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
    boxy_tables: BoxyTablesImage,
};

/// A child-side view over mapped shared memory. Most storage remains mapped,
/// while compact layout columns are reconstructed with `scratch_allocator`.
pub const ProgramView = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: []LIR.LirProcSpecId,
    platform_entrypoints: []PlatformEntrypoint,
    boxy_type_descs: []Program.BoxyTypeDesc,
    boxy_dicts: []Program.BoxyDict,
    boxy_adapters: []Program.BoxyAdapter,
    boxy_desc_refs: []Program.BoxyDescRef,
    boxy_dict_refs: []Program.BoxyDictRef,
    boxy_tag_variants: []Program.BoxyTagVariant,
    boxy_tag_payload_descs: []Program.BoxyTagPayloadDesc,
    boxy_field_names: []base.StringLiteral.Idx,
    boxy_adapt_steps: []Program.BoxyAdaptStep,
    boxy_payload_steps: []Program.BoxyPayloadStep,
    boxy_method_slots: []Program.BoxyMethodSlot,
    boxy_method_arg_layouts: []layout_mod.Idx,
    boxy_method_hidden_desc_sources: []Program.BoxyMethodHiddenDescSource,
    boxy_erased_arg_layouts: []layout_mod.Idx,
    boxy_erased_arg_desc_keys: []LIR.ErasedArgDescKey,
    boxy_erased_arg_desc_offsets: []LIR.ErasedArgDescOffset,
    boxy_erased_arg_desc_params: []LIR.ErasedArgDescParam,
    target_usize: base.target.TargetUsize,
    scratch_allocator: std.mem.Allocator,

    pub fn deinit(self: *ProgramView) void {
        deinitViewedLayouts(&self.layouts, self.scratch_allocator);
        self.* = undefined;
    }
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
    struct_fields: StructFieldsImage,
    struct_data: ArrayRef,
    tag_union_variants: TagUnionVariantsImage,
    tag_union_data: ArrayRef,

    fn fromStore(base_ptr: [*]align(1) const u8, image_size: usize, store: *const layout_mod.Store) ImageError!LayoutStoreImage {
        return .{
            .layouts = try arrayRef(base_ptr, image_size, store.layouts.items.items),
            .resolved_list_layouts = try arrayRef(base_ptr, image_size, store.resolved_list_layouts.items),
            .tuple_elems = try arrayRef(base_ptr, image_size, store.tuple_elems.items.items),
            .struct_fields = try StructFieldsImage.fromStore(base_ptr, image_size, &store.struct_fields),
            .struct_data = try arrayRef(base_ptr, image_size, store.struct_data.items.items),
            .tag_union_variants = try TagUnionVariantsImage.fromStore(base_ptr, image_size, &store.tag_union_variants),
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
            .struct_fields = try StructFieldsImage.copyFromStore(allocator, base_ptr, image_capacity, &store.struct_fields),
            .struct_data = try copyArrayRef(allocator, base_ptr, image_capacity, store.struct_data.items.items),
            .tag_union_variants = try TagUnionVariantsImage.copyFromStore(allocator, base_ptr, image_capacity, &store.tag_union_variants),
            .tag_union_data = try copyArrayRef(allocator, base_ptr, image_capacity, store.tag_union_data.items.items),
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
            .resolved_list_layouts = try arrayListFromRef(?layout_mod.Idx, base_ptr, image_size, self.resolved_list_layouts),
            .tuple_elems = try safeListFromRef(layout_mod.Idx, base_ptr, image_size, self.tuple_elems),
            .struct_fields = struct_fields,
            .struct_data = try safeListFromRef(layout_mod.StructData, base_ptr, image_size, self.struct_data),
            .tag_union_variants = tag_union_variants,
            .tag_union_data = try safeListFromRef(layout_mod.TagUnionData, base_ptr, image_size, self.tag_union_data),
            .interned_layouts = std.StringHashMap(layout_mod.Idx).init(allocator),
            .scratch_intern_key = .empty,
            .interned_recursive_graphs = std.StringHashMap(layout_mod.Idx).init(allocator),
            .target_usize = target_usize,
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

    fn fromStore(
        base_ptr: [*]align(1) const u8,
        image_size: usize,
        fields: *const layout_mod.StructField.SafeMultiList,
    ) ImageError!StructFieldsImage {
        return .{
            .indices = try arrayRef(base_ptr, image_size, fields.field(.index)),
            .layouts = try arrayRef(base_ptr, image_size, fields.field(.layout)),
            .is_padding = try arrayRef(base_ptr, image_size, fields.field(.is_padding)),
        };
    }

    fn copyFromStore(
        allocator: std.mem.Allocator,
        base_ptr: [*]align(1) const u8,
        image_capacity: usize,
        fields: *const layout_mod.StructField.SafeMultiList,
    ) CopyError!StructFieldsImage {
        return .{
            .indices = try copyArrayRef(allocator, base_ptr, image_capacity, fields.field(.index)),
            .layouts = try copyArrayRef(allocator, base_ptr, image_capacity, fields.field(.layout)),
            .is_padding = try copyArrayRef(allocator, base_ptr, image_capacity, fields.field(.is_padding)),
        };
    }

    fn view(
        self: StructFieldsImage,
        base_ptr: [*]align(1) u8,
        image_size: usize,
        allocator: std.mem.Allocator,
    ) ViewError!layout_mod.StructField.SafeMultiList {
        const indices = try sliceFromRef(u16, base_ptr, image_size, self.indices);
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

    fn fromStore(
        base_ptr: [*]align(1) const u8,
        image_size: usize,
        variants: *const layout_mod.TagUnionVariant.SafeMultiList,
    ) ImageError!TagUnionVariantsImage {
        return .{
            .payload_layouts = try arrayRef(base_ptr, image_size, variants.field(.payload_layout)),
        };
    }

    fn copyFromStore(
        allocator: std.mem.Allocator,
        base_ptr: [*]align(1) const u8,
        image_capacity: usize,
        variants: *const layout_mod.TagUnionVariant.SafeMultiList,
    ) CopyError!TagUnionVariantsImage {
        return .{
            .payload_layouts = try copyArrayRef(allocator, base_ptr, image_capacity, variants.field(.payload_layout)),
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

/// Public `BoxyTablesImage` declaration.
pub const BoxyTablesImage = extern struct {
    type_descs: ArrayRef,
    dicts: ArrayRef,
    adapters: ArrayRef,
    desc_refs: ArrayRef,
    dict_refs: ArrayRef,
    tag_variants: ArrayRef,
    tag_payload_descs: ArrayRef,
    field_names: ArrayRef,
    adapt_steps: ArrayRef,
    payload_steps: ArrayRef,
    method_slots: ArrayRef,
    method_arg_layouts: ArrayRef,
    method_hidden_desc_sources: ArrayRef,
    erased_arg_layouts: ArrayRef,
    erased_arg_desc_keys: ArrayRef,
    erased_arg_desc_offsets: ArrayRef,
    erased_arg_desc_params: ArrayRef,

    fn fromProgram(base_ptr: [*]align(1) const u8, image_size: usize, lowered: *const Program.Result) ImageError!BoxyTablesImage {
        return fromView(base_ptr, image_size, .{
            .type_descs = lowered.boxy_type_descs.items,
            .dicts = lowered.boxy_dicts.items,
            .adapters = lowered.boxy_adapters.items,
            .desc_refs = lowered.boxy_desc_refs.items,
            .dict_refs = lowered.boxy_dict_refs.items,
            .tag_variants = lowered.boxy_tag_variants.items,
            .tag_payload_descs = lowered.boxy_tag_payload_descs.items,
            .field_names = lowered.boxy_field_names.items,
            .adapt_steps = lowered.boxy_adapt_steps.items,
            .payload_steps = lowered.boxy_payload_steps.items,
            .method_slots = lowered.boxy_method_slots.items,
            .method_arg_layouts = lowered.boxy_method_arg_layouts.items,
            .method_hidden_desc_sources = lowered.boxy_method_hidden_desc_sources.items,
            .erased_arg_layouts = lowered.boxy_erased_arg_layouts.items,
            .erased_arg_desc_keys = lowered.boxy_erased_arg_desc_keys.items,
            .erased_arg_desc_offsets = lowered.boxy_erased_arg_desc_offsets.items,
            .erased_arg_desc_params = lowered.boxy_erased_arg_desc_params.items,
        });
    }

    fn copyFromProgram(
        allocator: std.mem.Allocator,
        base_ptr: [*]align(1) const u8,
        image_capacity: usize,
        lowered: *const Program.Result,
    ) CopyError!BoxyTablesImage {
        const tables = BoxyTablesView{
            .type_descs = lowered.boxy_type_descs.items,
            .dicts = lowered.boxy_dicts.items,
            .adapters = lowered.boxy_adapters.items,
            .desc_refs = lowered.boxy_desc_refs.items,
            .dict_refs = lowered.boxy_dict_refs.items,
            .tag_variants = lowered.boxy_tag_variants.items,
            .tag_payload_descs = lowered.boxy_tag_payload_descs.items,
            .field_names = lowered.boxy_field_names.items,
            .adapt_steps = lowered.boxy_adapt_steps.items,
            .payload_steps = lowered.boxy_payload_steps.items,
            .method_slots = lowered.boxy_method_slots.items,
            .method_arg_layouts = lowered.boxy_method_arg_layouts.items,
            .method_hidden_desc_sources = lowered.boxy_method_hidden_desc_sources.items,
            .erased_arg_layouts = lowered.boxy_erased_arg_layouts.items,
            .erased_arg_desc_keys = lowered.boxy_erased_arg_desc_keys.items,
            .erased_arg_desc_offsets = lowered.boxy_erased_arg_desc_offsets.items,
            .erased_arg_desc_params = lowered.boxy_erased_arg_desc_params.items,
        };
        return .{
            .type_descs = try copyArrayRef(allocator, base_ptr, image_capacity, tables.type_descs),
            .dicts = try copyArrayRef(allocator, base_ptr, image_capacity, tables.dicts),
            .adapters = try copyArrayRef(allocator, base_ptr, image_capacity, tables.adapters),
            .desc_refs = try copyArrayRef(allocator, base_ptr, image_capacity, tables.desc_refs),
            .dict_refs = try copyArrayRef(allocator, base_ptr, image_capacity, tables.dict_refs),
            .tag_variants = try copyArrayRef(allocator, base_ptr, image_capacity, tables.tag_variants),
            .tag_payload_descs = try copyArrayRef(allocator, base_ptr, image_capacity, tables.tag_payload_descs),
            .field_names = try copyArrayRef(allocator, base_ptr, image_capacity, tables.field_names),
            .adapt_steps = try copyArrayRef(allocator, base_ptr, image_capacity, tables.adapt_steps),
            .payload_steps = try copyArrayRef(allocator, base_ptr, image_capacity, tables.payload_steps),
            .method_slots = try copyArrayRef(allocator, base_ptr, image_capacity, tables.method_slots),
            .method_arg_layouts = try copyArrayRef(allocator, base_ptr, image_capacity, tables.method_arg_layouts),
            .method_hidden_desc_sources = try copyArrayRef(allocator, base_ptr, image_capacity, tables.method_hidden_desc_sources),
            .erased_arg_layouts = try copyArrayRef(allocator, base_ptr, image_capacity, tables.erased_arg_layouts),
            .erased_arg_desc_keys = try copyArrayRef(allocator, base_ptr, image_capacity, tables.erased_arg_desc_keys),
            .erased_arg_desc_offsets = try copyArrayRef(allocator, base_ptr, image_capacity, tables.erased_arg_desc_offsets),
            .erased_arg_desc_params = try copyArrayRef(allocator, base_ptr, image_capacity, tables.erased_arg_desc_params),
        };
    }

    fn fromView(base_ptr: [*]align(1) const u8, image_size: usize, tables: BoxyTablesView) ImageError!BoxyTablesImage {
        return .{
            .type_descs = try arrayRef(base_ptr, image_size, tables.type_descs),
            .dicts = try arrayRef(base_ptr, image_size, tables.dicts),
            .adapters = try arrayRef(base_ptr, image_size, tables.adapters),
            .desc_refs = try arrayRef(base_ptr, image_size, tables.desc_refs),
            .dict_refs = try arrayRef(base_ptr, image_size, tables.dict_refs),
            .tag_variants = try arrayRef(base_ptr, image_size, tables.tag_variants),
            .tag_payload_descs = try arrayRef(base_ptr, image_size, tables.tag_payload_descs),
            .field_names = try arrayRef(base_ptr, image_size, tables.field_names),
            .adapt_steps = try arrayRef(base_ptr, image_size, tables.adapt_steps),
            .payload_steps = try arrayRef(base_ptr, image_size, tables.payload_steps),
            .method_slots = try arrayRef(base_ptr, image_size, tables.method_slots),
            .method_arg_layouts = try arrayRef(base_ptr, image_size, tables.method_arg_layouts),
            .method_hidden_desc_sources = try arrayRef(base_ptr, image_size, tables.method_hidden_desc_sources),
            .erased_arg_layouts = try arrayRef(base_ptr, image_size, tables.erased_arg_layouts),
            .erased_arg_desc_keys = try arrayRef(base_ptr, image_size, tables.erased_arg_desc_keys),
            .erased_arg_desc_offsets = try arrayRef(base_ptr, image_size, tables.erased_arg_desc_offsets),
            .erased_arg_desc_params = try arrayRef(base_ptr, image_size, tables.erased_arg_desc_params),
        };
    }

    fn view(self: BoxyTablesImage, base_ptr: [*]align(1) u8, image_size: usize) ImageError!BoxyTablesView {
        return .{
            .type_descs = try sliceFromRef(Program.BoxyTypeDesc, base_ptr, image_size, self.type_descs),
            .dicts = try sliceFromRef(Program.BoxyDict, base_ptr, image_size, self.dicts),
            .adapters = try sliceFromRef(Program.BoxyAdapter, base_ptr, image_size, self.adapters),
            .desc_refs = try sliceFromRef(Program.BoxyDescRef, base_ptr, image_size, self.desc_refs),
            .dict_refs = try sliceFromRef(Program.BoxyDictRef, base_ptr, image_size, self.dict_refs),
            .tag_variants = try sliceFromRef(Program.BoxyTagVariant, base_ptr, image_size, self.tag_variants),
            .tag_payload_descs = try sliceFromRef(Program.BoxyTagPayloadDesc, base_ptr, image_size, self.tag_payload_descs),
            .field_names = try sliceFromRef(base.StringLiteral.Idx, base_ptr, image_size, self.field_names),
            .adapt_steps = try sliceFromRef(Program.BoxyAdaptStep, base_ptr, image_size, self.adapt_steps),
            .payload_steps = try sliceFromRef(Program.BoxyPayloadStep, base_ptr, image_size, self.payload_steps),
            .method_slots = try sliceFromRef(Program.BoxyMethodSlot, base_ptr, image_size, self.method_slots),
            .method_arg_layouts = try sliceFromRef(layout_mod.Idx, base_ptr, image_size, self.method_arg_layouts),
            .method_hidden_desc_sources = try sliceFromRef(Program.BoxyMethodHiddenDescSource, base_ptr, image_size, self.method_hidden_desc_sources),
            .erased_arg_layouts = try sliceFromRef(layout_mod.Idx, base_ptr, image_size, self.erased_arg_layouts),
            .erased_arg_desc_keys = try sliceFromRef(LIR.ErasedArgDescKey, base_ptr, image_size, self.erased_arg_desc_keys),
            .erased_arg_desc_offsets = try sliceFromRef(LIR.ErasedArgDescOffset, base_ptr, image_size, self.erased_arg_desc_offsets),
            .erased_arg_desc_params = try sliceFromRef(LIR.ErasedArgDescParam, base_ptr, image_size, self.erased_arg_desc_params),
        };
    }
};

/// Resolved slices for every descriptor-governed Boxy side table in an image.
pub const BoxyTablesView = struct {
    type_descs: []Program.BoxyTypeDesc,
    dicts: []Program.BoxyDict,
    adapters: []Program.BoxyAdapter,
    desc_refs: []Program.BoxyDescRef,
    dict_refs: []Program.BoxyDictRef,
    tag_variants: []Program.BoxyTagVariant,
    tag_payload_descs: []Program.BoxyTagPayloadDesc,
    field_names: []base.StringLiteral.Idx,
    adapt_steps: []Program.BoxyAdaptStep,
    payload_steps: []Program.BoxyPayloadStep,
    method_slots: []Program.BoxyMethodSlot,
    method_arg_layouts: []layout_mod.Idx,
    method_hidden_desc_sources: []Program.BoxyMethodHiddenDescSource,
    erased_arg_layouts: []layout_mod.Idx,
    erased_arg_desc_keys: []LIR.ErasedArgDescKey,
    erased_arg_desc_offsets: []LIR.ErasedArgDescOffset,
    erased_arg_desc_params: []LIR.ErasedArgDescParam,
};

/// The boxy runtime's table subset of a LIR image: the descriptor tables,
/// the committed layout store, and the string literal store the descriptors
/// index. Machine-code embedders view this to initialize a process-global
/// boxy runtime without decoding procs or statements.
pub const BoxySidecar = extern struct {
    layouts: LayoutStoreImage,
    strings: StringLiteralStoreImage,
    boxy_tables: BoxyTablesImage,

    /// The sidecar embedded in a full LIR image header.
    pub fn fromHeader(header: *const Header) BoxySidecar {
        return .{
            .layouts = header.layouts,
            .strings = header.store.strings,
            .boxy_tables = header.boxy_tables,
        };
    }

    /// Record sidecar offsets for a lowered program whose arrays live inside
    /// the buffer at `base_ptr`.
    pub fn fromProgram(
        base_ptr: [*]align(1) const u8,
        image_size: usize,
        lowered: *const Program.Result,
    ) ImageError!BoxySidecar {
        return .{
            .layouts = try LayoutStoreImage.fromStore(base_ptr, image_size, &lowered.layouts),
            .strings = try StringLiteralStoreImage.fromStore(base_ptr, image_size, &lowered.store.strings),
            .boxy_tables = try BoxyTablesImage.fromProgram(base_ptr, image_size, lowered),
        };
    }

    fn fromStores(
        base_ptr: [*]align(1) const u8,
        image_size: usize,
        layouts: *const layout_mod.Store,
        strings: *const base.StringLiteral.Store,
        tables: BoxyTablesView,
    ) ImageError!BoxySidecar {
        return .{
            .layouts = try LayoutStoreImage.fromStore(base_ptr, image_size, layouts),
            .strings = try StringLiteralStoreImage.fromStore(base_ptr, image_size, strings),
            .boxy_tables = try BoxyTablesImage.fromView(base_ptr, image_size, tables),
        };
    }

    /// Stores and table slices decoded from a mapped buffer. Ordinary arrays
    /// remain mapped in place; compact layout tables use the supplied allocator
    /// for target-native column storage. Keep both alive for the view's lifetime.
    pub const View = struct {
        layouts: layout_mod.Store,
        strings: base.StringLiteral.Store,
        tables: BoxyTablesView,
        scratch_allocator: std.mem.Allocator,

        pub fn deinit(self: *View) void {
            deinitViewedLayouts(&self.layouts, self.scratch_allocator);
            self.* = undefined;
        }
    };

    pub fn view(
        self: BoxySidecar,
        base_ptr: [*]align(1) u8,
        image_size: usize,
        target_usize: base.target.TargetUsize,
        allocator: std.mem.Allocator,
    ) ViewError!View {
        var layouts = try self.layouts.view(base_ptr, image_size, target_usize, allocator);
        errdefer deinitViewedLayouts(&layouts, allocator);
        return .{
            .layouts = layouts,
            .strings = try self.strings.view(base_ptr, image_size),
            .tables = try self.boxy_tables.view(base_ptr, image_size),
            .scratch_allocator = allocator,
        };
    }
};

/// A self-contained boxy sidecar: a byte buffer holding cloned copies of every
/// array a boxy runtime needs, plus a `BoxySidecar` whose offsets are relative
/// to `bytes.ptr`. Embedders that lower with a private allocator (whose arrays
/// are not already in the run image) build one of these, copy `bytes` verbatim
/// into the image, and view the sidecar with the copy's base pointer.
pub const SidecarBlob = struct {
    bytes: []align(16) u8,
    sidecar: BoxySidecar,

    pub fn deinit(self: *SidecarBlob, gpa: std.mem.Allocator) void {
        gpa.free(self.bytes);
        self.* = undefined;
    }
};

fn cloneStdArrayList(comptime T: type, gpa: std.mem.Allocator, list: std.ArrayList(T)) std.mem.Allocator.Error!std.ArrayList(T) {
    var out: std.ArrayList(T) = .empty;
    try out.ensureTotalCapacity(gpa, list.items.len);
    for (list.items) |item| {
        out.appendAssumeCapacity(item);
        collections.CompactWriter.zeroValuePadding(T, @ptrCast(&out.items[out.items.len - 1]));
    }
    return out;
}

fn cloneSafeList(comptime T: type, gpa: std.mem.Allocator, list: collections.SafeList(T)) std.mem.Allocator.Error!collections.SafeList(T) {
    return .{ .items = try cloneStdArrayList(T, gpa, list.items) };
}

fn cloneStructFields(
    gpa: std.mem.Allocator,
    source: *const layout_mod.StructField.SafeMultiList,
) std.mem.Allocator.Error!layout_mod.StructField.SafeMultiList {
    const indices = source.field(.index);
    const layouts = source.field(.layout);
    const padding = source.field(.is_padding);
    var result = try layout_mod.StructField.SafeMultiList.initCapacity(gpa, indices.len);
    for (indices, layouts, padding) |index, layout_idx, is_padding| {
        _ = result.appendAssumeCapacity(.{
            .index = index,
            .layout = layout_idx,
            .is_padding = is_padding,
        });
    }
    return result;
}

fn cloneTagUnionVariants(
    gpa: std.mem.Allocator,
    source: *const layout_mod.TagUnionVariant.SafeMultiList,
) std.mem.Allocator.Error!layout_mod.TagUnionVariant.SafeMultiList {
    const payload_layouts = source.field(.payload_layout);
    var result = try layout_mod.TagUnionVariant.SafeMultiList.initCapacity(gpa, payload_layouts.len);
    for (payload_layouts) |payload_layout| {
        _ = result.appendAssumeCapacity(.{ .payload_layout = payload_layout });
    }
    return result;
}

fn serializeSidecarInto(
    gpa: std.mem.Allocator,
    buffer: []align(16) u8,
    lowered: *const Program.Result,
) (ImageError || std.mem.Allocator.Error)!BoxySidecar {
    const layouts: layout_mod.Store = .{
        .allocator = gpa,
        .layouts = try cloneSafeList(layout_mod.Layout, gpa, lowered.layouts.layouts),
        .resolved_list_layouts = try cloneStdArrayList(?layout_mod.Idx, gpa, lowered.layouts.resolved_list_layouts),
        .tuple_elems = try cloneSafeList(layout_mod.Idx, gpa, lowered.layouts.tuple_elems),
        .struct_fields = try cloneStructFields(gpa, &lowered.layouts.struct_fields),
        .struct_data = try cloneSafeList(layout_mod.StructData, gpa, lowered.layouts.struct_data),
        .tag_union_variants = try cloneTagUnionVariants(gpa, &lowered.layouts.tag_union_variants),
        .tag_union_data = try cloneSafeList(layout_mod.TagUnionData, gpa, lowered.layouts.tag_union_data),
        .interned_layouts = std.StringHashMap(layout_mod.Idx).init(gpa),
        .scratch_intern_key = .empty,
        .interned_recursive_graphs = std.StringHashMap(layout_mod.Idx).init(gpa),
        .target_usize = lowered.layouts.target_usize,
    };
    const strings = try lowered.store.strings.clone(gpa);

    const type_descs = try cloneStdArrayList(Program.BoxyTypeDesc, gpa, lowered.boxy_type_descs);
    const dicts = try cloneStdArrayList(Program.BoxyDict, gpa, lowered.boxy_dicts);
    const adapters = try cloneStdArrayList(Program.BoxyAdapter, gpa, lowered.boxy_adapters);
    const desc_refs = try cloneStdArrayList(Program.BoxyDescRef, gpa, lowered.boxy_desc_refs);
    const dict_refs = try cloneStdArrayList(Program.BoxyDictRef, gpa, lowered.boxy_dict_refs);
    const tag_variants = try cloneStdArrayList(Program.BoxyTagVariant, gpa, lowered.boxy_tag_variants);
    const tag_payload_descs = try cloneStdArrayList(Program.BoxyTagPayloadDesc, gpa, lowered.boxy_tag_payload_descs);
    const field_names = try cloneStdArrayList(base.StringLiteral.Idx, gpa, lowered.boxy_field_names);
    const adapt_steps = try cloneStdArrayList(Program.BoxyAdaptStep, gpa, lowered.boxy_adapt_steps);
    const payload_steps = try cloneStdArrayList(Program.BoxyPayloadStep, gpa, lowered.boxy_payload_steps);
    const method_slots = try cloneStdArrayList(Program.BoxyMethodSlot, gpa, lowered.boxy_method_slots);
    const method_arg_layouts = try cloneStdArrayList(layout_mod.Idx, gpa, lowered.boxy_method_arg_layouts);
    const method_hidden_desc_sources = try cloneStdArrayList(Program.BoxyMethodHiddenDescSource, gpa, lowered.boxy_method_hidden_desc_sources);
    const erased_arg_layouts = try cloneStdArrayList(layout_mod.Idx, gpa, lowered.boxy_erased_arg_layouts);
    const erased_arg_desc_keys = try cloneStdArrayList(LIR.ErasedArgDescKey, gpa, lowered.boxy_erased_arg_desc_keys);
    const erased_arg_desc_offsets = try cloneStdArrayList(LIR.ErasedArgDescOffset, gpa, lowered.boxy_erased_arg_desc_offsets);
    const erased_arg_desc_params = try cloneStdArrayList(LIR.ErasedArgDescParam, gpa, lowered.boxy_erased_arg_desc_params);

    return BoxySidecar.fromStores(buffer.ptr, buffer.len, &layouts, &strings, .{
        .type_descs = type_descs.items,
        .dicts = dicts.items,
        .adapters = adapters.items,
        .desc_refs = desc_refs.items,
        .dict_refs = dict_refs.items,
        .tag_variants = tag_variants.items,
        .tag_payload_descs = tag_payload_descs.items,
        .field_names = field_names.items,
        .adapt_steps = adapt_steps.items,
        .payload_steps = payload_steps.items,
        .method_slots = method_slots.items,
        .method_arg_layouts = method_arg_layouts.items,
        .method_hidden_desc_sources = method_hidden_desc_sources.items,
        .erased_arg_layouts = erased_arg_layouts.items,
        .erased_arg_desc_keys = erased_arg_desc_keys.items,
        .erased_arg_desc_offsets = erased_arg_desc_offsets.items,
        .erased_arg_desc_params = erased_arg_desc_params.items,
    });
}

/// Serialize the boxy sidecar (layout store, string store, and boxy tables) of
/// a lowered program into a fresh self-contained buffer allocated from `gpa`.
/// The returned sidecar's offsets are relative to the buffer's base pointer.
pub fn buildSidecarBlob(
    gpa: std.mem.Allocator,
    lowered: *const Program.Result,
) (ImageError || std.mem.Allocator.Error)!SidecarBlob {
    var capacity: usize = 1 << 16;
    while (true) {
        const bytes = try gpa.alignedAlloc(u8, .@"16", capacity);
        @memset(bytes, 0);
        var fba = std.heap.FixedBufferAllocator.init(bytes);
        if (serializeSidecarInto(fba.allocator(), bytes, lowered)) |sidecar| {
            const compact = gpa.alignedAlloc(u8, .@"16", fba.end_index) catch |err| {
                gpa.free(bytes);
                return err;
            };
            @memcpy(compact, bytes[0..fba.end_index]);
            gpa.free(bytes);
            return .{ .bytes = compact, .sidecar = sidecar };
        } else |err| switch (err) {
            error.OutOfMemory => {
                gpa.free(bytes);
                capacity = std.math.mul(usize, capacity, 2) catch return error.OutOfMemory;
            },
            else => |image_err| {
                gpa.free(bytes);
                return image_err;
            },
        }
    }
}

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
    header.* = .{
        .magic = MAGIC,
        .format_version = FORMAT_VERSION,
        .image_size = image_size,
        .root_procs = try arrayRef(base_ptr, image_size, lowered.root_procs.items),
        .platform_entrypoints = try arrayRef(base_ptr, image_size, platform_entrypoints),
        .store = try LirStoreImage.fromStore(base_ptr, image_size, &lowered.store),
        .layouts = try LayoutStoreImage.fromStore(base_ptr, image_size, &lowered.layouts),
        .boxy_tables = try BoxyTablesImage.fromProgram(base_ptr, image_size, lowered),
    };
}

/// Exact copied LIR data awaiting its final image size and header.
pub const CopiedProgram = struct {
    image_capacity: usize,
    root_procs: ArrayRef,
    platform_entrypoints: ArrayRef,
    store: LirStoreImage,
    layouts: LayoutStoreImage,
    boxy_tables: BoxyTablesImage,

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
            .boxy_tables = self.boxy_tables,
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
        .boxy_tables = try BoxyTablesImage.copyFromProgram(allocator, base_ptr, image_capacity, lowered),
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

    // The view path constructs mutable container types (LirStore, Store)
    // whose slice fields are not const, even though the interpreter only
    // reads them. Cast once at the boundary so callers don't have to.
    const mutable_base: [*]align(1) u8 = @constCast(base_ptr);
    const boxy_tables = try header.boxy_tables.view(mutable_base, @intCast(header.image_size));

    var layouts = try header.layouts.view(mutable_base, @intCast(header.image_size), target_usize, allocator);
    errdefer deinitViewedLayouts(&layouts, allocator);
    return .{
        .store = try header.store.view(mutable_base, @intCast(header.image_size), allocator),
        .layouts = layouts,
        .root_procs = try sliceFromRef(LIR.LirProcSpecId, mutable_base, @intCast(header.image_size), header.root_procs),
        .platform_entrypoints = try sliceFromRef(PlatformEntrypoint, mutable_base, @intCast(header.image_size), header.platform_entrypoints),
        .boxy_type_descs = boxy_tables.type_descs,
        .boxy_dicts = boxy_tables.dicts,
        .boxy_adapters = boxy_tables.adapters,
        .boxy_desc_refs = boxy_tables.desc_refs,
        .boxy_dict_refs = boxy_tables.dict_refs,
        .boxy_tag_variants = boxy_tables.tag_variants,
        .boxy_tag_payload_descs = boxy_tables.tag_payload_descs,
        .boxy_field_names = boxy_tables.field_names,
        .boxy_adapt_steps = boxy_tables.adapt_steps,
        .boxy_payload_steps = boxy_tables.payload_steps,
        .boxy_method_slots = boxy_tables.method_slots,
        .boxy_method_arg_layouts = boxy_tables.method_arg_layouts,
        .boxy_method_hidden_desc_sources = boxy_tables.method_hidden_desc_sources,
        .boxy_erased_arg_layouts = boxy_tables.erased_arg_layouts,
        .boxy_erased_arg_desc_keys = boxy_tables.erased_arg_desc_keys,
        .boxy_erased_arg_desc_offsets = boxy_tables.erased_arg_desc_offsets,
        .boxy_erased_arg_desc_params = boxy_tables.erased_arg_desc_params,
        .target_usize = target_usize,
        .scratch_allocator = allocator,
    };
}

fn deinitViewedLayouts(layouts: *layout_mod.Store, allocator: std.mem.Allocator) void {
    layouts.struct_fields.deinit(allocator);
    layouts.tag_union_variants.deinit(allocator);
    layouts.interned_layouts.deinit();
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

/// Convert an intentional fixture-table position while preserving enum inference.
fn fixtureTableIndex(comptime index: u32) u32 {
    return index;
}

test "LIR image views empty and populated boxy tables" {
    const buffer = try std.testing.allocator.alignedAlloc(u8, .@"16", 1 << 20);
    defer std.testing.allocator.free(buffer);
    @memset(buffer, 0);

    var fba = std.heap.FixedBufferAllocator.init(buffer);
    const allocator = fba.allocator();

    const header = try allocator.create(Header);
    var lowered = try Program.Result.init(allocator, .u64);

    try fillHeaderInBuffer(header, buffer[0..].ptr, buffer.len, &lowered, &.{});
    var empty_view = try viewMappedImageWithAllocator(header, buffer[0..].ptr, buffer.len, .u64, allocator);
    defer empty_view.deinit();
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_type_descs.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_dicts.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_adapters.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_desc_refs.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_dict_refs.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_tag_variants.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_tag_payload_descs.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_adapt_steps.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_payload_steps.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_method_slots.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_method_arg_layouts.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_method_hidden_desc_sources.len);
    try std.testing.expectEqual(@as(usize, 0), empty_view.boxy_erased_arg_layouts.len);

    try lowered.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    try lowered.boxy_payload_steps.append(allocator, .{ .dynamic = .{
        .op = .copy,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    } });
    try lowered.boxy_method_arg_layouts.append(allocator, .zst);
    try lowered.boxy_erased_arg_layouts.append(allocator, .u64);
    try lowered.boxy_method_hidden_desc_sources.append(allocator, .{ .slot = 0 });
    try lowered.boxy_dict_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    try lowered.boxy_tag_variants.append(allocator, .{
        .name = try lowered.store.insertString("Ok"),
        .discriminant = 0,
        .payload_count = 1,
        .payload_layout = .zst,
        .payload_descs = .{ .start = 0, .len = 1 },
    });
    try lowered.boxy_tag_payload_descs.append(allocator, .{
        .payload_index = 0,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    });
    try lowered.boxy_method_slots.append(allocator, .{
        .method = @enumFromInt(fixtureTableIndex(0)),
        .proc = @enumFromInt(fixtureTableIndex(0)),
        .adapter = .{
            .arg_layouts = .{ .start = 0, .len = 1 },
            .arg_descs = .{ .start = 0, .len = 1 },
            .call_descs = .{ .start = 0, .len = 1 },
            .nested_dicts = .{ .start = 0, .len = 1 },
            .hidden_desc_sources = .{ .start = 0, .len = 1 },
        },
    });
    try lowered.boxy_type_descs.append(allocator, .{
        .payload_layout = .zst,
        .contains_refcounted = true,
        .nested_descs = .{ .start = 0, .len = 1 },
        .tag_variants = .{ .start = 0, .len = 1 },
        .copy_plan = .{ .start = 0, .len = 1 },
        .presence_slot_present_discriminant = 1,
        .inspect_method = @enumFromInt(fixtureTableIndex(0)),
    });
    try lowered.boxy_dicts.append(allocator, .{
        .method_slots = .{ .start = 0, .len = 1 },
        .hidden_descs = .{ .start = 0, .len = 1 },
        .nested_dicts = .{ .start = 0, .len = 1 },
    });
    try lowered.boxy_adapt_steps.append(allocator, .{ .copy_bytes = .{
        .source_offset = 0,
        .target_offset = 8,
        .layout_idx = .str,
    } });
    try lowered.boxy_adapters.append(allocator, .{
        .kind = .host_to_boxy,
        .source_layout = .str,
        .target_layout = .str,
        .steps = .{ .start = 0, .len = 1 },
        .consumes_source = false,
        .produces_owned_result = true,
    });
    const struct_field_idx = try lowered.layouts.struct_fields.append(allocator, .{
        .index = 7,
        .layout = .str,
        .is_padding = true,
    });
    const tag_variant_idx = try lowered.layouts.tag_union_variants.append(allocator, .{
        .payload_layout = .u64,
    });
    const ret_desc_local = try lowered.store.addLocal(.{ .layout_idx = .opaque_ptr });
    const ret_value = try lowered.store.addLocal(.{
        .layout_idx = .str,
        .boxy_desc = .{ .local = ret_desc_local },
    });
    const ret_stmt = try lowered.store.addCFStmt(.{ .ret = .{ .value = ret_value } });
    const proc_id = try lowered.store.addProcSpec(.{
        .name = lowered.store.freshSyntheticSymbol(),
        .args = try lowered.store.addLocalSpan(&.{ret_desc_local}),
        .body = ret_stmt,
        .ret_layout = .str,
        .ret_desc = .{ .local = ret_desc_local },
    });

    try fillHeaderInBuffer(header, buffer[0..].ptr, buffer.len, &lowered, &.{});
    var populated_view = try viewMappedImageWithAllocator(header, buffer[0..].ptr, buffer.len, .u64, allocator);
    defer populated_view.deinit();
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_type_descs.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_dicts.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_adapters.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_desc_refs.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_dict_refs.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_tag_variants.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_tag_payload_descs.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_adapt_steps.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_payload_steps.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_method_slots.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_method_arg_layouts.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_method_hidden_desc_sources.len);
    try std.testing.expectEqual(@as(usize, 1), populated_view.boxy_erased_arg_layouts.len);
    try std.testing.expect(populated_view.boxy_type_descs[0].contains_refcounted);
    try std.testing.expectEqual(@as(?u16, 1), populated_view.boxy_type_descs[0].presence_slot_present_discriminant);
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(populated_view.boxy_type_descs[0].inspect_method.?));
    try std.testing.expectEqual(@as(u16, 0), populated_view.boxy_tag_variants[0].discriminant);
    try std.testing.expectEqualStrings("Ok", populated_view.store.getString(populated_view.boxy_tag_variants[0].name));
    try std.testing.expectEqual(@as(u32, 0), populated_view.boxy_tag_payload_descs[0].payload_index);
    try std.testing.expectEqual(Program.BoxyAdapterKind.host_to_boxy, populated_view.boxy_adapters[0].kind);
    try std.testing.expectEqual(layout_mod.Idx.str, populated_view.boxy_adapt_steps[0].copy_bytes.layout_idx);
    try std.testing.expectEqual(Program.BoxyPayloadOp.copy, populated_view.boxy_payload_steps[0].dynamic.op);
    try std.testing.expectEqual(layout_mod.Idx.zst, populated_view.boxy_method_arg_layouts[0]);
    try std.testing.expectEqual(layout_mod.Idx.u64, populated_view.boxy_erased_arg_layouts[0]);
    try std.testing.expectEqual(Program.BoxySpan{ .start = 0, .len = 1 }, populated_view.boxy_method_slots[0].adapter.call_descs);
    try std.testing.expectEqual(@as(u32, 0), populated_view.boxy_method_hidden_desc_sources[0].slot);
    try std.testing.expectEqual(@as(u16, 7), populated_view.layouts.struct_fields.fieldItem(.index, struct_field_idx));
    try std.testing.expectEqual(layout_mod.Idx.str, populated_view.layouts.struct_fields.fieldItem(.layout, struct_field_idx));
    try std.testing.expect(populated_view.layouts.struct_fields.fieldItem(.is_padding, struct_field_idx));
    try std.testing.expectEqual(layout_mod.Idx.u64, populated_view.layouts.tag_union_variants.fieldItem(.payload_layout, tag_variant_idx));
    try std.testing.expectEqual(LIR.BoxyDescRef{ .local = ret_desc_local }, populated_view.store.getProcSpec(proc_id).ret_desc.?);
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
        /// A per-field-and-index distinctive *value* of `T`. Filling raw bytes
        /// instead would write bit patterns no value of the type can hold—an
        /// `Idx` is 28 bits in a four-byte slot and a `bool` is one of two
        /// bytes—and a copy that moves values, as the MultiArrayList columns
        /// do, is free not to reproduce them.
        fn distinctValue(comptime T: type, ordinal: usize) T {
            if (T == bool) return ordinal & 1 == 1;
            const info = @typeInfo(T);
            if (info == .int) return @truncate(ordinal);
            if (info == .@"enum") return @enumFromInt(@as(info.@"enum".tag_type, @truncate(ordinal)));
            @compileError("distinctValue: unhandled field type " ++ @typeName(T));
        }
        /// Build a populated `SafeMultiList(T)` backed by the fixed buffer,
        /// giving every column its own value sequence so a dropped or swapped
        /// field is detectable after view.
        fn multiList(comptime T: type, alloc: std.mem.Allocator, count: usize, seed: u8) std.mem.Allocator.Error!collections.SafeMultiList(T) {
            var mal: std.MultiArrayList(T) = .{};
            try mal.resize(alloc, count);
            const slice = mal.slice();
            inline for (std.meta.fields(T), 0..) |field, field_index| {
                const column = slice.items(@field(std.MultiArrayList(T).Field, field.name));
                for (column, 0..) |*value, i| {
                    value.* = distinctValue(field.type, seed + field_index * 64 + i);
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
    var lowered = try Program.Result.init(source_allocator, target_usize);
    lowered.store = store;
    lowered.layouts = layouts;
    lowered.root_procs = .{ .items = root_procs, .capacity = root_procs.len };
    const copied = try copyProgramIntoBuffer(fba, base_ptr, buffer.len, &lowered, entrypoints);
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
