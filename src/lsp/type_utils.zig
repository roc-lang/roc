//! Type manipulation utilities for the LSP module.
//!
//! This module provides reusable functions for working with types:
//! - Unwrapping type aliases to get underlying content
//! - Extracting record fields from types
//! - Extracting base type names from formatted type strings
//! - Querying alias information

const std = @import("std");
const types = @import("types");
const base = @import("base");

const TypeStore = types.Store;
const Content = types.Content;
const Var = types.Var;
const Record = types.Record;
const Presence = types.RecordField.Presence;
const Ident = base.Ident;

/// Result of unwrapping type aliases
pub const UnwrapResult = struct {
    /// The content after unwrapping aliases
    content: Content,
    /// How many alias layers were unwrapped
    depth: usize,
};

/// Unwrap type aliases to get the underlying content.
/// Returns the innermost content after following alias chains.
/// Stops after max_depth iterations to prevent infinite loops.
pub fn unwrapAliases(type_store: *const TypeStore, type_var: Var, max_depth: usize) UnwrapResult {
    var resolved = type_store.resolveVar(type_var);
    var content = resolved.desc.content;
    var depth: usize = 0;

    while (depth < max_depth) : (depth += 1) {
        if (std.meta.activeTag(content) != .alias) break;
        const backing_var = type_store.getAliasBackingVar(content.alias);
        resolved = type_store.resolveVar(backing_var);
        content = resolved.desc.content;
    }

    return .{
        .content = content,
        .depth = depth,
    };
}

/// Information about a single record field
pub const RecordFieldInfo = struct {
    name: Ident.Idx,
    type_var: Var,
    presence: Presence,
};

/// Iterator over record fields.
/// This avoids allocations by providing an iterator interface over internal storage.
///
/// The type of a field lives on its presence's second axis, so the iterator
/// derives `type_var` from the presence.
pub const RecordFieldsIterator = struct {
    names: []const Ident.Idx,
    presences: []const Presence,
    index: usize = 0,

    /// Get the next field, or null if exhausted
    pub fn next(self: *RecordFieldsIterator) ?RecordFieldInfo {
        if (self.index >= self.names.len) return null;
        const idx = self.index;
        self.index += 1;
        const presence = self.presences[idx];
        return .{
            .name = self.names[idx],
            .type_var = presence.typeVar(),
            .presence = presence,
        };
    }

    /// Reset the iterator to the beginning
    pub fn reset(self: *RecordFieldsIterator) void {
        self.index = 0;
    }

    /// Get the total number of fields
    pub fn len(self: RecordFieldsIterator) usize {
        return self.names.len;
    }

    /// Check if there are more fields
    pub fn hasNext(self: RecordFieldsIterator) bool {
        return self.index < self.names.len;
    }
};

/// Get an iterator over record fields.
/// The iterator references internal storage - do not modify the type store while iterating.
pub fn getRecordFieldsIterator(type_store: *const TypeStore, record: Record) RecordFieldsIterator {
    const fields_slice = type_store.getRecordFieldsSlice(record.fields);
    return .{
        .names = fields_slice.items(.name),
        .presences = fields_slice.items(.presence),
    };
}

/// Check if a content is an alias and get the backing var.
/// Returns the backing var if the content is an alias, null otherwise.
pub fn getAliasBackingVar(type_store: *const TypeStore, content: Content) ?Var {
    return if (std.meta.activeTag(content) == .alias) type_store.getAliasBackingVar(content.alias) else null;
}

// Tests

test "RecordFieldsIterator" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var type_store = try TypeStore.initCapacity(allocator, 8, 4);
    defer type_store.deinit();

    const names = [_]Ident.Idx{
        .{ .idx = 1, .attributes = .{ .effectful = false, .ignored = false, .reserved = false } },
        .{ .idx = 2, .attributes = .{ .effectful = false, .ignored = false, .reserved = false } },
        .{ .idx = 3, .attributes = .{ .effectful = false, .ignored = false, .reserved = false } },
    };
    const first_var = try type_store.freshFromContent(.err);
    const second_var = try type_store.freshFromContent(.err);
    const third_var = try type_store.freshFromContent(.err);
    const ext_var = try type_store.freshFromContent(.{ .structure = .empty_record });
    const opt_presence = try type_store.fresh();
    const fields = try type_store.appendRecordFields(&.{
        .{ .name = names[0], .presence = .required(first_var) },
        .{ .name = names[1], .presence = .unknown(opt_presence, second_var) },
        .{ .name = names[2], .presence = .required(third_var) },
    });

    var iter = getRecordFieldsIterator(&type_store, .{ .fields = fields, .ext = ext_var });

    try testing.expectEqual(@as(usize, 3), iter.len());
    try testing.expect(iter.hasNext());

    // First field
    const field1 = iter.next().?;
    try testing.expectEqual(@as(u29, 1), field1.name.idx);
    try testing.expectEqual(first_var, field1.type_var);
    try testing.expectEqual(null, field1.presence.presenceVar());

    // Second field
    const field2 = iter.next().?;
    try testing.expectEqual(@as(u29, 2), field2.name.idx);
    try testing.expectEqual(second_var, field2.type_var);
    try testing.expectEqual(opt_presence, field2.presence.presenceVar());

    // Third field
    const field3 = iter.next().?;
    try testing.expectEqual(@as(u29, 3), field3.name.idx);
    try testing.expectEqual(third_var, field3.type_var);
    try testing.expectEqual(null, field3.presence.presenceVar());

    // No more fields
    try testing.expect(iter.next() == null);
    try testing.expect(!iter.hasNext());

    // Reset and iterate again
    iter.reset();
    try testing.expect(iter.hasNext());
    try testing.expect(iter.next() != null);
}
