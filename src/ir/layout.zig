//! Shared logical executable layouts carried through IR and committed once at
//! the `IR -> LIR/layout` boundary.

const std = @import("std");
const layout_mod = @import("layout");

pub const Ref = layout_mod.GraphRef;
pub const Graph = layout_mod.Graph;
pub const Node = layout_mod.GraphNode;
pub const NodeId = layout_mod.GraphNodeId;
pub const Field = layout_mod.GraphField;
pub const FieldSpan = layout_mod.GraphFieldSpan;
pub const RefSpan = layout_mod.GraphRefSpan;

test "ir layout tests" {
    std.testing.refAllDecls(@This());
}
