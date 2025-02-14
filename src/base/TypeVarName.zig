//! The name of a type variable, e.g. `foo` in `List foo`.
const std = @import("std");
const name_store = @import("./name_store.zig");
const collections = @import("../collections.zig");

const NameStore = name_store.NameStore;
const SmallStringInterner = collections.SmallStringInterner;

/// The index for a type var name in a TypeVarName.Store.
pub const Idx = enum(u32) { _ };

/// An interner for a type variable name.
///
/// A thin wrapper around a small string interner that
/// allows for typed IDs of type variable names which can't be
/// interchanged with other interned string IDs.
pub const Store = NameStore(Idx);
