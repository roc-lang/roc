//! Memory layout representations and stores for values in running Roc programs.
//!
//! This module provides the core layout system used by the Roc compiler to determine
//! how values are represented in memory. It includes:
//!
//! - Layout definitions for scalars, containers, structs (records/tuples), and closures
//! - A layout store that manages layout instances and their dependencies
//! - Canonical graph interning and RC-helper planning for ordinary data
//!
//! See the Layout Store for how these representations actually get created
//! (using type and target information from previous steps in compilation).
//!
//! Ordinary data layout is fully determined here and shared across compiler
//! phases.

const std = @import("std");

// Re-export the main layout types and functionality
pub const Layout = @import("layout.zig").Layout;
pub const LayoutTag = @import("layout.zig").LayoutTag;
pub const LayoutUnion = @import("layout.zig").LayoutUnion;
pub const Idx = @import("layout.zig").Idx;
pub const Scalar = @import("layout.zig").Scalar;
pub const ScalarTag = @import("layout.zig").ScalarTag;
pub const ScalarUnion = @import("layout.zig").ScalarUnion;
pub const Closure = @import("layout.zig").Closure;
// Unified struct types (records and tuples are both structs at the layout level)
pub const StructField = @import("layout.zig").StructField;
pub const StructLayout = @import("layout.zig").StructLayout;
pub const StructIdx = @import("layout.zig").StructIdx;
pub const StructData = @import("layout.zig").StructData;
// Backwards-compat aliases
pub const RecordField = @import("layout.zig").RecordField;
pub const RecordLayout = @import("layout.zig").RecordLayout;
pub const RecordIdx = @import("layout.zig").RecordIdx;
pub const RecordData = @import("layout.zig").RecordData;
pub const TupleField = @import("layout.zig").TupleField;
pub const TupleFieldLayout = @import("layout.zig").TupleFieldLayout;
pub const TupleLayout = @import("layout.zig").TupleLayout;
pub const TupleIdx = @import("layout.zig").TupleIdx;
pub const TupleData = @import("layout.zig").TupleData;
pub const TagUnionLayout = @import("layout.zig").TagUnionLayout;
pub const TagUnionIdx = @import("layout.zig").TagUnionIdx;
pub const TagUnionData = @import("layout.zig").TagUnionData;
pub const TagUnionVariant = @import("layout.zig").TagUnionVariant;
pub const ClosureLayout = @import("layout.zig").ClosureLayout;
pub const RocAlignment = @import("layout.zig").RocAlignment;
pub const SizeAlign = @import("layout.zig").SizeAlign;

// Re-export Info types
pub const ListInfo = @import("layout.zig").ListInfo;
pub const BoxInfo = @import("layout.zig").BoxInfo;
pub const StructInfo = @import("layout.zig").StructInfo;
// Backwards-compat aliases
pub const RecordInfo = @import("layout.zig").RecordInfo;
pub const TupleInfo = @import("layout.zig").TupleInfo;
pub const TagUnionInfo = @import("layout.zig").TagUnionInfo;
pub const ScalarInfo = @import("layout.zig").ScalarInfo;

// Re-export store functionality
pub const Store = @import("store.zig").Store;
pub const Graph = @import("graph.zig").Graph;
pub const GraphNode = @import("graph.zig").Node;
pub const GraphNodeId = @import("graph.zig").NodeId;
pub const GraphRef = @import("graph.zig").Ref;
pub const GraphField = @import("graph.zig").Field;
pub const GraphFieldSpan = @import("graph.zig").FieldSpan;
pub const GraphRefSpan = @import("graph.zig").RefSpan;
pub const RcOp = @import("rc_helper.zig").RcOp;
pub const RcHelperKey = @import("rc_helper.zig").HelperKey;
pub const RcHelperPlan = @import("rc_helper.zig").Plan;
pub const RcStructPlan = @import("rc_helper.zig").StructPlan;
pub const RcTagUnionPlan = @import("rc_helper.zig").TagUnionPlan;
pub const RcListPlan = @import("rc_helper.zig").ListPlan;
pub const RcBoxPlan = @import("rc_helper.zig").BoxPlan;
pub const RcFieldPlan = @import("rc_helper.zig").FieldPlan;
pub const RcHelperResolver = @import("rc_helper.zig").Resolver;
pub const RcIncrefFn = @import("rc_helper.zig").RcIncrefFn;
pub const RcDecrefFn = @import("rc_helper.zig").RcDecrefFn;
pub const RcFreeFn = @import("rc_helper.zig").RcFreeFn;

test "layout tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("layout.zig"));
    std.testing.refAllDecls(@import("graph.zig"));
    std.testing.refAllDecls(@import("rc_helper.zig"));
    std.testing.refAllDecls(@import("store.zig"));
}
