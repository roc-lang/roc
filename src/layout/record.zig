//! Record layout computation and processing.
//! Handles the conversion of record types into their memory layout representation.

const std = @import("std");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("./layout.zig");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const work = @import("./work.zig");

const Var = types.Var;
const Layout = layout.Layout;
const Idx = layout.Idx;
const RecordField = layout.RecordField;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const Parent = work.PendingContainer;
const FieldWorkItem = work.FieldWorkItem;

/// Result of processing a record during layout computation
pub const RecordProcessingResult = struct {
    record_idx: Idx, // Note: this will be undefined until the record is actually created
    field_count: u32,
    fields_start: usize,
};

/// Context for processing records, to avoid passing function pointers
pub const ProcessContext = struct {
    allocator: std.mem.Allocator,
    layouts: *std.ArrayListUnmanaged(Layout),
    record_fields: *RecordFieldSafeMultiList,

    pub fn insertLayout(self: *ProcessContext, layout_: Layout) std.mem.Allocator.Error!Idx {
        const idx = self.layouts.items.len;
        try self.layouts.append(self.allocator, layout_);
        return Idx{
            .attributes = .{ ._padding = 0 },
            .idx = @intCast(idx),
        };
    }

    /// Create a record layout after verifying it has at least one field
    pub fn createRecordLayout(self: *ProcessContext, fields_start: usize, field_count: u32) !Idx {
        if (field_count == 0) {
            return error.EmptyRecord;
        }

        const non_empty_range = try collections.NonEmptyRange.init(@intCast(fields_start), field_count);
        return self.insertLayout(Layout{ .record = .{ .fields = non_empty_range } });
    }
};

/// Process a record type and return the record layout index and field count
pub fn processRecord(
    var_store: *const types_store.Store,
    record: types.Record,
    fields_start: usize,
) std.mem.Allocator.Error!RecordProcessingResult {
    // Don't create the record layout yet - we'll create it after processing fields
    // to ensure we never create an empty record

    // Count fields from main record
    var field_count: u32 = 0;
    const field_iter = var_store.iterMulti(record.fields);
    while (field_iter.next()) |_| {
        field_count += 1;
    }

    return RecordProcessingResult{
        .record_idx = undefined, // Will be set later when we actually create the record
        .field_count = field_count,
        .fields_start = fields_start,
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

/// Add a field to a record's field list, checking for duplicates
/// Note: This doesn't update the record layout itself, as that may not exist yet
pub fn addRecordField(
    allocator: std.mem.Allocator,
    record_fields: *RecordFieldSafeMultiList,
    fields_start: usize,
    field_count: u32,
    field_name: Ident.Idx,
    field_layout_idx: Idx,
) !enum { added, duplicate_match, duplicate_mismatch } {

    // Check if this field already exists in the range
    const end = fields_start + field_count;
    for (record_fields.items[fields_start..end]) |existing_field| {
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
