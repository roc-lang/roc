//! Names paired with the logical field and tag positions committed by lowering.
//! Host marshallers consume these schemas instead of reconstructing layout order.

const std = @import("std");

/// A record field name and the logical position lowering committed for it.
pub const RuntimeRecordFieldSchema = struct {
    name: []const u8,
    logical_index: u32,
};

/// The committed field positions of one named record type.
pub const RuntimeRecordSchema = struct {
    type_name: []const u8,
    fields: []const RuntimeRecordFieldSchema,

    /// The committed logical position of `field_name`, or null if the record has no such field.
    pub fn fieldLogicalIndex(self: RuntimeRecordSchema, field_name: []const u8) ?u32 {
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, field_name)) return field.logical_index;
        }
        return null;
    }
};

/// A tag name and the discriminant lowering committed for it.
pub const RuntimeTagSchema = struct {
    name: []const u8,
    discriminant: u32,
};

/// The committed discriminants of one named tag union type.
pub const RuntimeTagUnionSchema = struct {
    type_name: []const u8,
    tags: []const RuntimeTagSchema,

    /// The committed discriminant of `tag_name`, or null if the union has no such tag.
    pub fn tagDiscriminant(self: RuntimeTagUnionSchema, tag_name: []const u8) ?u32 {
        for (self.tags) |tag| {
            if (std.mem.eql(u8, tag.name, tag_name)) return tag.discriminant;
        }
        return null;
    }
};

/// Owns every record and tag union schema one lowering run commits.
pub const RuntimeSchemaStore = struct {
    allocator: std.mem.Allocator,
    records: std.ArrayList(RuntimeRecordSchema),
    tag_unions: std.ArrayList(RuntimeTagUnionSchema),

    /// An empty store whose schemas will be owned by `allocator`.
    pub fn init(allocator: std.mem.Allocator) RuntimeSchemaStore {
        return .{ .allocator = allocator, .records = .empty, .tag_unions = .empty };
    }

    /// Frees every schema and the names it owns.
    pub fn deinit(self: *RuntimeSchemaStore) void {
        for (self.records.items) |schema| {
            for (schema.fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema.fields);
            self.allocator.free(schema.type_name);
        }
        for (self.tag_unions.items) |schema| {
            for (schema.tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema.tags);
            self.allocator.free(schema.type_name);
        }
        self.tag_unions.deinit(self.allocator);
        self.records.deinit(self.allocator);
    }
};
