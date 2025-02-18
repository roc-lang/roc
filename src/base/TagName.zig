//! The name of a tag, e.g. `Foo` in `Foo("abc", 123)`.
const std = @import("std");
const name_store = @import("./name_store.zig");
const collections = @import("../collections.zig");

const NameStore = name_store.NameStore;
const SmallStringInterner = collections.SmallStringInterner;

/// The index for a tag name in a TagName.Interner.
pub const Idx = enum(u32) { _ };

/// An interner for a tag name.
///
/// A thin wrapper around a small string interner that
/// allows for typed IDs of tag names which can't be
/// interchanged with other interned string IDs.
pub const Store = NameStore(Idx);
