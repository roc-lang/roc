//! Layout uses a manual stack instead of recursion, in order to be stack-safe.
//! This data structure tracks pending work between one iteration and the next.

const std = @import("std");
const types = @import("types");
const layout = @import("./layout.zig");
const Ident = @import("base").Ident;

/// Key to identify a nominal type by its identity (ident + origin module)
/// Used for cycle detection in recursive nominal types where different vars
/// can reference the same nominal type definition.
pub const NominalKey = struct {
    ident_idx: Ident.Idx,
    origin_module: Ident.Idx,
};

/// Work queue for layout computation, tracking pending and resolved containers
pub const Work = struct {
    pending_containers: std.MultiArrayList(PendingContainerItem),
    pending_record_fields: std.MultiArrayList(types.RecordField),
    resolved_record_fields: std.MultiArrayList(ResolvedRecordField),
    pending_tags: std.MultiArrayList(types.Tag),
    resolved_tags: std.MultiArrayList(ResolvedTag),
    pending_tuple_fields: std.MultiArrayList(TupleField),
    resolved_tuple_fields: std.MultiArrayList(ResolvedTupleField),
    /// Vars currently being processed - used to detect recursive type references
    in_progress_vars: std.AutoArrayHashMap(types.Var, void),
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
    };

    pub const PendingContainerItem = struct { var_: types.Var, container: PendingContainer };

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

        return .{
            .pending_containers = pending_containers,
            .pending_record_fields = pending_record_fields,
            .resolved_record_fields = resolved_record_fields,
            .pending_tags = pending_tags,
            .resolved_tags = resolved_tags,
            .pending_tuple_fields = pending_tuple_fields,
            .resolved_tuple_fields = resolved_tuple_fields,
            .in_progress_vars = std.AutoArrayHashMap(types.Var, void).init(allocator),
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
        self.in_progress_vars.deinit();
        self.in_progress_nominals.deinit();
    }

    // NOTE: We do NOT have a clearRetainingCapacity function because all work fields
    // must persist across recursive addTypeVar calls (e.g., when processing tag union
    // variant payloads). Fields are cleaned up individually when types finish processing:
    // - pending_containers: pop() when container layout is finalized
    // - in_progress_vars: swapRemove() when type is cached
    // - in_progress_nominals: swapRemove() when nominal type is updated
    // - pending_record_fields, pending_tuple_fields: pop() when field is resolved
    // - resolved_record_fields, resolved_tuple_fields: used then left for next call
    // - pending_tags, resolved_tags: shrinkRetainingCapacity() via defer
    //
    // Example problem case that would occur if we cleared fields:
    //   { tag: Str, attrs: List([StringAttr(Str, Str), BoolAttr(Str, Bool)]) }
    // When processing this record, we push record fields. Then when processing
    // the tag union element of the List, we make recursive addTypeVar calls for
    // variant payloads. If we cleared pending_record_fields, the outer record's
    // field tracking would be destroyed, causing unreachable panics.
};
