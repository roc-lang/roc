const std = @import("std");
const types = @import("../types/types.zig");
const layout = @import("./layout.zig");
const Ident = @import("../base/Ident.zig");

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
        record: struct { num_fields: u32, pending_fields: u32, resolved_fields_start: u32 },
    };

    pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) Work {
        return .{
            .pending_containers = std.ArrayListUnmanaged(PendingContainer).init(allocator, capacity),
            .pending_record_fields = std.MultiArrayList(types.RecordField).init(allocator, capacity),
            .resolved_record_fields = std.MultiArrayList(ResolvedRecordField).init(allocator, capacity),
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
