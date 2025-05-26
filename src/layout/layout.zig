const std = @import("std");
const types = @import("../types/types.zig");

// Attributes for layout indices
pub const Attributes = packed struct(u3) {
    // Reserved for future use
    _padding: u3 = 0,
};

// Layout index with attributes
pub const Idx = packed struct(u32) {
    attributes: Attributes,
    idx: u29,
};

// Zero-sized types must have been eliminated prior to creating a Layout.
// No Layout should ever represent a zero-sized type, such as an empty tag union.
// Once a type has been converted to a layout, there is no longer any distinction
// between nominal and structural types, there's just memory.
// Also, records have become tuples (aka structs).
pub const Layout = union(enum) {
    str,
    box: Idx,
    list: Idx,
    tuple: Tuple,
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
    func: Func, // TODO how does the closure fit into here?
    tagged_union: TagUnion,
    host_opaque,
};

// Placeholder types - to be defined
pub const Tuple = struct {
    // TODO: implement
};

pub const Func = struct {
    // TODO: implement
};

pub const TagUnion = struct {
    // TODO: implement
};
