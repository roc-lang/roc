//! Layout uses a manual stack instead of recursion, in order to be stack-safe.
//! This data structure tracks pending work between one iteration and the next.

const std = @import("std");
const types = @import("../types/types.zig");
const layout = @import("./layout.zig");
const Ident = @import("../base/Ident.zig");

/// Work queue for layout computation, tracking pending and resolved containers
pub const Work = struct {
    pending_containers: std.ArrayListUnmanaged(PendingContainer),
    pending_record_fields: std.MultiArrayList(types.RecordField),
    resolved_record_fields: std.MultiArrayList(ResolvedRecordField),

    pub const ResolvedRecordField = struct {
        field_name: Ident.Idx,
        field_idx: layout.Idx,
    };

    pub const PendingContainer = union(enum) {
        box,
        list,
        record: PendingRecord,
    };

    pub const PendingRecord = struct {
        num_fields: u32,
        pending_fields: u32,
        resolved_fields_start: u32,
    };

    pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !Work {
        var pending_containers = std.ArrayListUnmanaged(PendingContainer){};
        try pending_containers.ensureTotalCapacity(allocator, capacity);

        var pending_record_fields = std.MultiArrayList(types.RecordField){};
        try pending_record_fields.ensureTotalCapacity(allocator, capacity);

        var resolved_record_fields = std.MultiArrayList(ResolvedRecordField){};
        try resolved_record_fields.ensureTotalCapacity(allocator, capacity);

        return .{
            .pending_containers = pending_containers,
            .pending_record_fields = pending_record_fields,
            .resolved_record_fields = resolved_record_fields,
        };
    }

    pub fn deinit(self: *Work, allocator: std.mem.Allocator) void {
        self.pending_containers.deinit(allocator);
        self.pending_record_fields.deinit(allocator);
        self.resolved_record_fields.deinit(allocator);
    }

    pub fn clearRetainingCapacity(self: *Work) void {
        self.pending_containers.clearRetainingCapacity();
        self.pending_record_fields.clearRetainingCapacity();
        self.resolved_record_fields.clearRetainingCapacity();
    }
};
