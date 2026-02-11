//! Memory layout representations and stores for values in running Roc programs.
//!
//! This module provides the core layout system used by the Roc compiler to determine
//! how values are represented in memory. It includes:
//!
//! - Layout definitions for scalars, containers, records, tuples, and closures
//! - A layout store that manages layout instances and their dependencies
//! - Work queue management for stack-safe layout computation
//!
//! See the Layout Store for how these representations actually get created
//! (using type and target information from previous steps in compilation).

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
pub const RecordField = @import("layout.zig").RecordField;
pub const FieldNameIdx = @import("layout.zig").FieldNameIdx;
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
pub const RecordInfo = @import("layout.zig").RecordInfo;
pub const TupleInfo = @import("layout.zig").TupleInfo;
pub const TagUnionInfo = @import("layout.zig").TagUnionInfo;
pub const ScalarInfo = @import("layout.zig").ScalarInfo;

// Re-export store functionality
pub const Store = @import("store.zig").Store;
pub const ModuleVarKey = @import("store.zig").ModuleVarKey;

// Re-export work queue functionality
pub const Work = @import("work.zig").Work;
pub const work = @import("work.zig");

test "layout tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("layout.zig"));
    std.testing.refAllDecls(@import("store.zig"));
    std.testing.refAllDecls(@import("work.zig"));
    std.testing.refAllDecls(@import("store_test.zig"));
}
