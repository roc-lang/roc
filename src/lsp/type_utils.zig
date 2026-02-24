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
const TypeIdent = types.TypeIdent;
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
        switch (content) {
            .alias => |alias| {
                const backing_var = type_store.getAliasBackingVar(alias);
                resolved = type_store.resolveVar(backing_var);
                content = resolved.desc.content;
            },
            else => break,
        }
    }

    return .{
        .content = content,
        .depth = depth,
    };
}

/// Unwrap aliases and return record if found, null otherwise.
/// This is a convenience function that combines alias unwrapping with record extraction.
pub fn unwrapToRecord(type_store: *const TypeStore, type_var: Var, max_depth: usize) ?Record {
    const result = unwrapAliases(type_store, type_var, max_depth);
    return result.content.unwrapRecord();
}

/// Information about a single record field
pub const RecordFieldInfo = struct {
    name: Ident.Idx,
    type_var: Var,
};

/// Iterator over record fields.
/// This avoids allocations by providing an iterator interface over internal storage.
pub const RecordFieldsIterator = struct {
    names: []const Ident.Idx,
    vars: []const Var,
    index: usize = 0,

    /// Get the next field, or null if exhausted
    pub fn next(self: *RecordFieldsIterator) ?RecordFieldInfo {
        if (self.index >= self.names.len) {
            return null;
        }
        const field = RecordFieldInfo{
            .name = self.names[self.index],
            .type_var = self.vars[self.index],
        };
        self.index += 1;
        return field;
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
        .vars = fields_slice.items(.var_),
    };
}

/// Extract the base type name from a formatted type string.
/// E.g., "List a" → "List", "Dict k v" → "Dict", "Foo[Bar]" → "Foo", "Foo(a)" → "Foo"
///
/// This is useful for getting the primary type name from a type string
/// that may include type parameters or other decorations.
pub fn extractBaseTypeName(type_str: []const u8) []const u8 {
    // Skip leading whitespace
    var start: usize = 0;
    while (start < type_str.len and (type_str[start] == ' ' or type_str[start] == '\t')) {
        start += 1;
    }

    // Find end of the type name (stop at space, bracket, paren, brace, or end)
    var end = start;
    while (end < type_str.len) {
        const c = type_str[end];
        if (c == ' ' or c == '[' or c == '(' or c == '{' or c == '<') break;
        end += 1;
    }

    return type_str[start..end];
}

/// Get the alias type identifier if the content is an alias.
/// Returns null if the content is not an alias.
pub fn getAliasIdent(content: Content) ?TypeIdent {
    return switch (content) {
        .alias => |alias| alias.ident,
        else => null,
    };
}

/// Check if a content is an alias and get the backing var.
/// Returns the backing var if the content is an alias, null otherwise.
pub fn getAliasBackingVar(type_store: *const TypeStore, content: Content) ?Var {
    return switch (content) {
        .alias => |alias| type_store.getAliasBackingVar(alias),
        else => null,
    };
}

/// Get the backing var for a type variable if it's an alias.
/// This resolves the type variable first, then checks if it's an alias.
/// Returns null if the resolved content is not an alias.
pub fn getTypeVarAliasBackingVar(type_store: *const TypeStore, type_var: Var) ?Var {
    const resolved = type_store.resolveVar(type_var);
    return getAliasBackingVar(type_store, resolved.desc.content);
}

/// Check if a type variable resolves to a record type (directly or through aliases).
/// Returns the record if found, null otherwise.
pub fn isRecordType(type_store: *const TypeStore, type_var: Var, max_alias_depth: usize) ?Record {
    return unwrapToRecord(type_store, type_var, max_alias_depth);
}

/// Check if content is an error type
pub fn isErrorContent(content: Content) bool {
    return content == .err;
}

/// Check if a type variable resolves to an error type
pub fn isErrorType(type_store: *const TypeStore, type_var: Var) bool {
    const resolved = type_store.resolveVar(type_var);
    return isErrorContent(resolved.desc.content);
}

// Tests

test "extractBaseTypeName basic" {
    const testing = std.testing;

    try testing.expectEqualStrings("List", extractBaseTypeName("List a"));
    try testing.expectEqualStrings("Dict", extractBaseTypeName("Dict k v"));
    try testing.expectEqualStrings("Str", extractBaseTypeName("Str"));
    try testing.expectEqualStrings("Foo", extractBaseTypeName("Foo[Bar]"));
    try testing.expectEqualStrings("Foo", extractBaseTypeName("Foo(a)"));
    try testing.expectEqualStrings("Foo", extractBaseTypeName("Foo{bar}"));
    try testing.expectEqualStrings("Foo", extractBaseTypeName("Foo<T>"));
}

test "extractBaseTypeName with whitespace" {
    const testing = std.testing;

    try testing.expectEqualStrings("List", extractBaseTypeName("  List a"));
    try testing.expectEqualStrings("Dict", extractBaseTypeName("\tDict k v"));
    try testing.expectEqualStrings("Str", extractBaseTypeName("  \t  Str"));
}

test "extractBaseTypeName empty and edge cases" {
    const testing = std.testing;

    try testing.expectEqualStrings("", extractBaseTypeName(""));
    try testing.expectEqualStrings("", extractBaseTypeName("   "));
    try testing.expectEqualStrings("", extractBaseTypeName("(a)"));
    try testing.expectEqualStrings("", extractBaseTypeName("[tag]"));
}

test "RecordFieldsIterator" {
    const testing = std.testing;

    // Create test data
    const names = [_]Ident.Idx{
        .{ .idx = 1, .attributes = .{ .effectful = false, .ignored = false, .reassignable = false } },
        .{ .idx = 2, .attributes = .{ .effectful = false, .ignored = false, .reassignable = false } },
        .{ .idx = 3, .attributes = .{ .effectful = false, .ignored = false, .reassignable = false } },
    };
    const vars = [_]Var{
        @enumFromInt(10),
        @enumFromInt(20),
        @enumFromInt(30),
    };

    var iter = RecordFieldsIterator{
        .names = &names,
        .vars = &vars,
    };

    try testing.expectEqual(@as(usize, 3), iter.len());
    try testing.expect(iter.hasNext());

    // First field
    const field1 = iter.next().?;
    try testing.expectEqual(@as(u29, 1), field1.name.idx);
    try testing.expectEqual(@as(u32, 10), @intFromEnum(field1.type_var));

    // Second field
    const field2 = iter.next().?;
    try testing.expectEqual(@as(u29, 2), field2.name.idx);
    try testing.expectEqual(@as(u32, 20), @intFromEnum(field2.type_var));

    // Third field
    const field3 = iter.next().?;
    try testing.expectEqual(@as(u29, 3), field3.name.idx);
    try testing.expectEqual(@as(u32, 30), @intFromEnum(field3.type_var));

    // No more fields
    try testing.expect(iter.next() == null);
    try testing.expect(!iter.hasNext());

    // Reset and iterate again
    iter.reset();
    try testing.expect(iter.hasNext());
    try testing.expect(iter.next() != null);
}
