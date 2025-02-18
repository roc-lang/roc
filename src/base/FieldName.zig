//! The name of a field in a record, e.g. `foo` in `{ foo: 123 }`.
const std = @import("std");
const name_store = @import("./name_store.zig");
const collections = @import("../collections.zig");

const NameStore = name_store.NameStore;
const SmallStringInterner = collections.SmallStringInterner;

/// The index for a record field name in a FieldName.Store.
pub const Idx = enum(u32) { _ };

/// An interner for a record field name.
///
/// A thin wrapper around a small string interner that
/// allows for typed IDs of record field names which can't be
/// interchanged with other interned string IDs.
pub const Store = NameStore(Idx);
