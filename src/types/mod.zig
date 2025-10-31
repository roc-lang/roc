//! Type system definitions and utilities for the Roc compiler.
//!
//! This module re-exports core type definitions from the types/ directory.
//! It serves as the main interface for type-related functionality throughout
//! the compiler, including type aliases, content types, function types,
//! and nominal types.

const std = @import("std");

pub const types = @import("types.zig");
pub const store = @import("store.zig");
pub const instantiate = @import("instantiate.zig");
pub const generalize = @import("generalize.zig");
pub const import_mapping = @import("import_mapping.zig");

pub const TypeWriter = @import("TypeWriter.zig");

pub const Alias = types.Alias;
pub const Flex = types.Flex;
pub const Rigid = types.Rigid;
pub const Content = types.Content;
pub const FlatType = types.FlatType;
pub const Func = types.Func;
pub const NominalType = types.NominalType;
pub const Num = types.Num;
pub const Record = types.Record;
pub const RecordField = types.RecordField;
pub const Tag = types.Tag;
pub const TagUnion = types.TagUnion;
pub const Tuple = types.Tuple;
pub const Var = types.Var;
pub const TypeIdent = types.TypeIdent;
pub const Descriptor = types.Descriptor;
pub const TwoRecordFields = types.TwoRecordFields;
pub const TwoTags = types.TwoTags;
pub const Rank = types.Rank;
pub const Mark = types.Mark;
pub const TypeScope = types.TypeScope;
pub const VarMap = types.VarMap;
pub const StaticDispatchConstraint = types.StaticDispatchConstraint;
pub const TwoStaticDispatchConstraints = types.TwoStaticDispatchConstraints;

pub const Slot = store.Slot;
pub const ResolvedVarDesc = store.ResolvedVarDesc;
pub const ResolvedVarDescs = store.ResolvedVarDescs;
pub const Store = store.Store;
pub const DescStoreIdx = store.DescStoreIdx;

test {
    std.testing.refAllDecls(@import("test/test_rigid_instantiation.zig"));
}
