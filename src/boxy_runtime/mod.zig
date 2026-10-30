//! The Boxy runtime: type descriptors, their composition and interning, and
//! the descriptor-guided copy, incref, decref, equality, hashing, and
//! inspection walkers (design.md "`.boxy` Runtime Lowering").
//!
//! The interpreter calls this Zig API directly. Compiled code calls the
//! `roc_desc_*` C entry points in `exports.zig`, which `static_lib.zig`
//! exports from the native runtime object. This module imports only
//! `builtins`, the std-only `layout_shape`, and `value_rules`, so it compiles
//! without libc and without the rest of the compiler.

pub const desc = @import("desc.zig");
pub const compose = @import("compose.zig");
pub const walk = @import("walk.zig");
pub const exports = @import("exports.zig");
pub const compiled = @import("compiled.zig");

pub const TypeDesc = desc.TypeDesc;
pub const Field = desc.Field;
pub const Variant = desc.Variant;
pub const Name = desc.Name;
pub const NameId = desc.NameId;
pub const labelId = desc.labelId;
pub const nominalId = desc.nominalId;
pub const Methods = desc.Methods;
pub const Kind = desc.Kind;
pub const Scalar = desc.Scalar;
pub const Flags = desc.Flags;
pub const PointerWidth = desc.PointerWidth;
pub const ExternLayout = desc.ExternLayout;

pub const Cache = compose.Cache;
pub const Value = walk.Value;

test "boxy runtime tests" {
    @import("std").testing.refAllDecls(@This());
    _ = @import("test.zig");
}
