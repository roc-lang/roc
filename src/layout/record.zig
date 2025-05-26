const std = @import("std");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("./layout.zig");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");

const Var = types.Var;
const Layout = layout.Layout;
const Idx = layout.Idx;
const RecordField = layout.RecordField;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;

pub const Parent = union(enum) {
    none,
    box_elem: struct { idx: Idx },
    list_elem: struct { idx: Idx },
    record_field: struct { idx: Idx, field_name: Ident.Idx },
};

pub const RecordProcessingResult = struct {
    record_idx: Idx,
    field_count: u32,
};

pub const FieldWorkItem = struct {
    var_: Var,
    field_name: Ident.Idx,
};

/// Context for processing records, to avoid passing function pointers
pub const ProcessContext = struct {
    allocator: std.mem.Allocator,
    layouts: *std.ArrayListUnmanaged(Layout),

    pub fn insertLayout(self: *ProcessContext, layout_: Layout) std.mem.Allocator.Error!Idx {
        const idx = self.layouts.items.len;
        try self.layouts.append(self.allocator, layout_);
        return Idx{
            .attributes = .{ ._padding = 0 },
            .idx = @intCast(idx),
        };
    }
};

/// Process a record type and return the record layout index and field count
pub fn processRecord(
    ctx: *ProcessContext,
    var_store: *const types_store.Store,
    record: types.Record,
    fields_start: usize,
) std.mem.Allocator.Error!RecordProcessingResult {
    // Create a placeholder record layout
    const placeholder_range = RecordFieldSafeMultiList.Range{
        .start = @intCast(fields_start),
        .count = 0,
    };
    const record_idx = try ctx.insertLayout(Layout{ .record = .{ .fields = placeholder_range } });

    // Count fields from main record
    var field_count: u32 = 0;
    const field_iter = var_store.iterMulti(record.fields);
    while (field_iter.next()) |_| {
        field_count += 1;
    }

    return RecordProcessingResult{
        .record_idx = record_idx,
        .field_count = field_count,
    };
}

/// Get field work items for a record
pub fn getFieldWorkItems(
    allocator: std.mem.Allocator,
    var_store: *const types_store.Store,
    record: types.Record,
) std.mem.Allocator.Error!std.ArrayList(FieldWorkItem) {
    var items = std.ArrayList(FieldWorkItem).init(allocator);
    errdefer items.deinit();

    const field_iter = var_store.iterMulti(record.fields);
    while (field_iter.next()) |field| {
        try items.append(.{
            .var_ = var_store.resolveVar(field.var_).var_,
            .field_name = field.name,
        });
    }

    return items;
}

/// Process record extension and return additional field work items
pub fn processExtension(
    var_store: *const types_store.Store,
    ext_var: Var,
    out_items: *std.ArrayList(FieldWorkItem),
) !union(enum) { done, continue_with: Var, invalid } {
    const ext_resolved = var_store.resolveVar(ext_var);
    const ext_content = ext_resolved.desc.content;

    switch (ext_content) {
        .structure => |ext_flat_type| switch (ext_flat_type) {
            .empty_record => {
                // Empty extension, nothing to do
                return .done;
            },
            .record => |ext_record| {
                // Extension is another record, collect its fields
                const ext_field_iter = var_store.iterMulti(ext_record.fields);
                while (ext_field_iter.next()) |field| {
                    try out_items.append(.{
                        .var_ = var_store.resolveVar(field.var_).var_,
                        .field_name = field.name,
                    });
                }

                // Continue with the extension's extension
                return .{ .continue_with = var_store.resolveVar(ext_record.ext).var_ };
            },
            else => {
                // Invalid extension type
                return .invalid;
            },
        },
        .alias => |alias| {
            // Follow the alias
            return .{ .continue_with = var_store.resolveVar(alias.backing_var).var_ };
        },
        else => {
            // Invalid extension type
            return .invalid;
        },
    }
}

/// Add a field to a record, checking for duplicates
pub fn addRecordField(
    allocator: std.mem.Allocator,
    record_fields: *RecordFieldSafeMultiList,
    record_layout: *Layout,
    field_name: Ident.Idx,
    field_layout_idx: Idx,
) !enum { added, duplicate_match, duplicate_mismatch } {

    // Check if this field already exists
    const existing_fields = record_fields.slice(record_layout.record.fields);
    for (existing_fields) |existing_field| {
        if (existing_field.name == field_name) {
            // Field already exists, check if types match
            if (existing_field.layout != field_layout_idx) {
                return .duplicate_mismatch;
            }
            // Types match, skip adding duplicate
            return .duplicate_match;
        }
    }

    // Field doesn't exist yet, add it
    const field = RecordField{
        .name = field_name,
        .layout = field_layout_idx,
    };
    try record_fields.append(allocator, field);

    // Update the record's field count
    record_layout.record.fields.count += 1;

    return .added;
}

/// Handle an empty record based on its parent context
pub fn handleEmptyRecord(
    parent: Parent,
    layouts: *std.ArrayListUnmanaged(Layout),
) enum { error_zero_sized, box_zero_sized, list_zero_sized, record_field_dropped } {
    switch (parent) {
        .none => {
            // Root level empty record is an error
            return .error_zero_sized;
        },
        .box_elem => |box_parent| {
            // Replace parent with box_zero_sized
            layouts.items[box_parent.idx.idx] = .box_zero_sized;
            return .box_zero_sized;
        },
        .list_elem => |list_parent| {
            // Replace parent with list_zero_sized
            layouts.items[list_parent.idx.idx] = .list_zero_sized;
            return .list_zero_sized;
        },
        .record_field => {
            // The parent record will drop this field when checking isZeroSized
            return .record_field_dropped;
        },
    }
}
