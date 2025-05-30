//! Memory layout representation for Roc types.
//! Converts high-level type information into concrete memory layouts used for code generation.

const std = @import("std");
const types = @import("../types/types.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");

/// Attributes for layout indices
pub const Attributes = packed struct(u3) {
    // Reserved for future use
    _padding: u3 = 0,
};

/// Layout index with attributes
pub const Idx = packed struct(u32) {
    attributes: Attributes,
    idx: u29,
};

/// Memory layout representation for Roc types.
/// Zero-sized types must have been eliminated prior to creating a Layout.
/// No Layout should ever represent a zero-sized type, such as an empty tag union.
/// Once a type has been converted to a layout, there is no longer any distinction
/// between nominal and structural types, there's just memory.
/// Also, records have become tuples (aka structs).
pub const Layout = union(enum) {
    str,
    box: Idx,
    box_zero_sized, // e.g. a Box({}) - this can come up, so we need a special implementation for it.
    list: Idx,
    list_zero_sized, // e.g. a List({}) - this can come up, so we need to make a special implementation for it.
    record: Record,
    tuple: Tuple,
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
    func: Func, // TODO how does the closure fit into here?
    tagged_union: TagUnion,
    host_opaque,
};

/// Record field layout
pub const RecordField = struct {
    /// The name of the field
    name: Ident.Idx,
    /// The layout of the field's value
    layout: Idx,

    /// A SafeMultiList for storing record fields
    pub const SafeMultiList = collections.SafeMultiList(RecordField);
};

/// Record layout
pub const Record = struct {
    fields: collections.NonEmptyRange,
    // Note: no extension variable here - layouts are concrete

    pub fn getFields(self: Record) RecordField.SafeMultiList.Range {
        return RecordField.SafeMultiList.Range{
            .start = self.fields.start,
            .count = self.fields.count,
        };
    }
};

/// Tuple layout (ordered collection of fields)
pub const Tuple = struct {
    // TODO: implement
};

/// Function layout
pub const Func = struct {
    // TODO: implement
};

/// Tagged union layout
pub const TagUnion = struct {
    // TODO: implement
};
