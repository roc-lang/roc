// There are
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
    num: Num,
    func: Func, // TODO how does the closure fit into here?
    tagged_union: TagUnion,
};
