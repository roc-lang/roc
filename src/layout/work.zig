//! Layout uses a manual stack instead of recursion, in order to be stack-safe.
//! This data structure tracks pending work between one iteration and the next.

const std = @import("std");
const types = @import("types");
const layout = @import("./layout.zig");
const Ident = @import("base").Ident;

/// Key to identify a type variable in a specific module.
/// Used to distinguish type vars with the same index across different modules.
pub const ModuleVarKey = packed struct {
    module_idx: u32,
    var_: types.Var,
};

/// Key to identify a nominal type by its identity (ident + origin module)
/// Used for cycle detection in recursive nominal types where different vars
/// can reference the same nominal type definition.
pub const NominalKey = struct {
    ident_idx: Ident.Idx,
    origin_module: Ident.Idx,
};

/// Work queue for layout computation, tracking pending and resolved containers.
///
/// Layout computation uses an iterative work queue instead of recursion to be stack-safe.
/// Container types (records, tuples, tag unions) push their fields/variants to pending
/// lists, then process them one at a time. When a field/variant layout is computed,
/// it moves to the resolved list. When all are resolved, the container is finalized.
pub const Work = struct {
    pending_containers: std.MultiArrayList(PendingContainerItem),
    pending_record_fields: std.MultiArrayList(types.RecordField),
    resolved_record_fields: std.MultiArrayList(ResolvedRecordField),
    pending_tags: std.MultiArrayList(types.Tag),
    resolved_tags: std.MultiArrayList(ResolvedTag),
    pending_tuple_fields: std.MultiArrayList(TupleField),
    resolved_tuple_fields: std.MultiArrayList(ResolvedTupleField),
    /// Tag union variants waiting for payload layout computation
    pending_tag_union_variants: std.MultiArrayList(TagUnionVariant),
    /// Tag union variants whose payload layouts have been computed
    resolved_tag_union_variants: std.MultiArrayList(ResolvedTagUnionVariant),
    /// Vars currently being processed - used to detect recursive type references.
    /// Keyed by (module_idx, var) to distinguish vars across modules.
    in_progress_vars: std.AutoArrayHashMap(ModuleVarKey, void),
    /// Nominal types currently being processed - used to detect recursive nominal types.
    /// Unlike in_progress_vars, this tracks by nominal identity (ident + origin_module)
    /// because recursive references to the same nominal type may have different vars.
    /// The value contains the nominal's var (for cache lookup) and its backing var
    /// (to know when to update the placeholder).
    in_progress_nominals: std.AutoArrayHashMap(NominalKey, NominalProgress),

    /// Info about a nominal type being processed
    pub const NominalProgress = struct {
        nominal_var: types.Var,
        backing_var: types.Var,
        /// The type arguments of this nominal stored as a range into the types store.
        /// Using a range (start index + count) instead of a slice avoids dangling
        /// pointers if the underlying vars storage is reallocated while processing
        /// nested types. The range can be re-sliced when needed.
        /// Used to distinguish different instantiations of the same nominal type.
        /// e.g., Try(Str, Str) vs Try((Try(Str, Str), U64), Str) have different type args.
        type_args_range: types.Var.SafeList.Range,
        /// True if a recursive cycle was detected while processing this nominal type.
        /// This is set when we encounter the same nominal type during its own processing.
        is_recursive: bool = false,
    };

    /// A container being processed. The var_ is optional because synthetic tuples
    /// (created for multi-arg tag union variants) don't have a meaningful var to cache.
    /// module_idx tracks which module the var belongs to for correct in_progress_vars removal.
    pub const PendingContainerItem = struct { var_: ?types.Var, module_idx: u32, container: PendingContainer };

    /// Tuple field for layout work - similar to RecordField but with index instead of name.
    /// We need to explicitly record the index because zero-sized tuple fields might have
    /// been dropped, and yet we need to know what the original indices were for debuginfo.
    pub const TupleField = struct {
        index: u16,
        var_: types.Var,
    };

    pub const ResolvedTag = struct {
        field_name: Ident.Idx,
        field_idx: layout.Idx,
    };

    pub const ResolvedRecordField = struct {
        field_name: Ident.Idx,
        field_idx: layout.Idx,
    };

    pub const ResolvedTupleField = struct {
        field_index: u16,
        field_idx: layout.Idx,
    };

    pub const PendingContainer = union(enum) {
        box,
        list,
        record: PendingRecord,
        tuple: PendingTuple,
        tag_union: PendingTagUnion,
    };

    /// A tag union variant whose payload layout is pending computation.
    /// Used in iterative tag union processing to avoid stack overflow.
    pub const TagUnionVariant = struct {
        /// Index of this variant in the sorted tag list (for correct ordering in final layout)
        index: u16,
        /// Type vars for this variant's payload args. For single-arg variants, this has
        /// length 1. For multi-arg variants like `Point(1, 2)`, this contains all args
        /// which will be processed as a tuple.
        args: types.Var.SafeList.Range,
    };

    /// A tag union variant whose payload layout has been computed.
    pub const ResolvedTagUnionVariant = struct {
        /// Index of this variant in the sorted tag list
        index: u16,
        /// The computed layout for this variant's payload
        layout_idx: layout.Idx,
    };

    /// Tracks a tag union being processed iteratively.
    /// Sits on `pending_containers` while its variants are being resolved.
    pub const PendingTagUnion = struct {
        /// Total number of variants in this tag union
        num_variants: u32,
        /// Number of variants with payloads still waiting to be processed
        pending_variants: u32,
        /// Index into `resolved_tag_union_variants` where this tag union's resolved variants start
        resolved_variants_start: u32,
        /// Pre-computed discriminant layout (u8/u16/u32 based on variant count)
        discriminant_layout: layout.Idx,
    };

    pub const PendingRecord = struct {
        num_fields: u32,
        pending_fields: u32,
        resolved_fields_start: u32,
    };

    pub const PendingTuple = struct {
        num_fields: u32,
        pending_fields: u32,
        resolved_fields_start: u32,
    };

    pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !Work {
        var pending_containers = std.MultiArrayList(PendingContainerItem){};
        try pending_containers.ensureTotalCapacity(allocator, capacity);

        var pending_record_fields = std.MultiArrayList(types.RecordField){};
        try pending_record_fields.ensureTotalCapacity(allocator, capacity);

        var resolved_record_fields = std.MultiArrayList(ResolvedRecordField){};
        try resolved_record_fields.ensureTotalCapacity(allocator, capacity);

        var pending_tags = std.MultiArrayList(types.Tag){};
        try pending_tags.ensureTotalCapacity(allocator, capacity);

        var resolved_tags = std.MultiArrayList(ResolvedTag){};
        try resolved_tags.ensureTotalCapacity(allocator, capacity);

        var pending_tuple_fields = std.MultiArrayList(TupleField){};
        try pending_tuple_fields.ensureTotalCapacity(allocator, capacity);

        var resolved_tuple_fields = std.MultiArrayList(ResolvedTupleField){};
        try resolved_tuple_fields.ensureTotalCapacity(allocator, capacity);

        var pending_tag_union_variants = std.MultiArrayList(TagUnionVariant){};
        try pending_tag_union_variants.ensureTotalCapacity(allocator, capacity);

        var resolved_tag_union_variants = std.MultiArrayList(ResolvedTagUnionVariant){};
        try resolved_tag_union_variants.ensureTotalCapacity(allocator, capacity);

        return .{
            .pending_containers = pending_containers,
            .pending_record_fields = pending_record_fields,
            .resolved_record_fields = resolved_record_fields,
            .pending_tags = pending_tags,
            .resolved_tags = resolved_tags,
            .pending_tuple_fields = pending_tuple_fields,
            .resolved_tuple_fields = resolved_tuple_fields,
            .pending_tag_union_variants = pending_tag_union_variants,
            .resolved_tag_union_variants = resolved_tag_union_variants,
            .in_progress_vars = std.AutoArrayHashMap(ModuleVarKey, void).init(allocator),
            .in_progress_nominals = std.AutoArrayHashMap(NominalKey, NominalProgress).init(allocator),
        };
    }

    pub fn deinit(self: *Work, allocator: std.mem.Allocator) void {
        self.pending_containers.deinit(allocator);
        self.pending_record_fields.deinit(allocator);
        self.resolved_record_fields.deinit(allocator);
        self.pending_tags.deinit(allocator);
        self.resolved_tags.deinit(allocator);
        self.pending_tuple_fields.deinit(allocator);
        self.resolved_tuple_fields.deinit(allocator);
        self.pending_tag_union_variants.deinit(allocator);
        self.resolved_tag_union_variants.deinit(allocator);
        self.in_progress_vars.deinit();
        self.in_progress_nominals.deinit();
    }

    // NOTE: We do NOT have a clearRetainingCapacity function because all work fields
    // must persist across nested container processing. Fields are cleaned up individually
    // when types finish processing:
    // - pending_containers: pop() when container layout is finalized
    // - in_progress_vars: swapRemove() when type is cached
    // - in_progress_nominals: swapRemove() when nominal type is updated
    // - pending_record_fields, pending_tuple_fields: pop() when field is resolved
    // - resolved_record_fields, resolved_tuple_fields: shrinkRetainingCapacity() when done
    // - pending_tags, resolved_tags: shrinkRetainingCapacity() via defer
    // - pending_tag_union_variants, resolved_tag_union_variants: same as record/tuple fields
    //
    // Example problem case that would occur if we cleared fields:
    //   { tag: Str, attrs: List([StringAttr(Str, Str), BoolAttr(Str, Bool)]) }
    // When processing this record, we push record fields. Then when processing
    // the tag union element of the List, we push tag union variants. If we cleared
    // pending_record_fields, the outer record's field tracking would be destroyed.
};
