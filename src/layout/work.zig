//! Layout uses a manual stack instead of recursion, in order to be stack-safe.
//! This data structure tracks pending work between one iteration and the next.

const std = @import("std");
const types = @import("types");
const layout = @import("./layout.zig");
const Ident = @import("base").Ident;

/// Work queue for layout computation, tracking pending and resolved containers
pub const Work = struct {
    pending_containers: std.MultiArrayList(PendingContainerItem),
    pending_record_fields: std.MultiArrayList(types.RecordField),
    resolved_record_fields: std.MultiArrayList(ResolvedRecordField),
    pending_tags: std.MultiArrayList(types.Tag),
    resolved_tags: std.MultiArrayList(ResolvedTag),
    pending_tuple_fields: std.MultiArrayList(TupleField),
    resolved_tuple_fields: std.MultiArrayList(ResolvedTupleField),

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
    }

    pub fn clearRetainingCapacity(self: *Work) void {
        self.pending_containers.clearRetainingCapacity();
        self.pending_record_fields.clearRetainingCapacity();
        self.resolved_record_fields.clearRetainingCapacity();
        self.pending_tags.clearRetainingCapacity();
        self.resolved_tags.clearRetainingCapacity();
        self.pending_tuple_fields.clearRetainingCapacity();
        self.resolved_tuple_fields.clearRetainingCapacity();
    }
};
