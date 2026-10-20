//! Value rules shared by both runtime lowering strategies.
//!
//! `Str.inspect` rendering, structural equality, structural hashing, and the
//! selection of a `to_inspect` override depend on a value's shape, not on how
//! a lowering strategy reached its type. Monotype lowering (`.lss`), Boxy
//! lowering (`.boxy`), and the Boxy runtime each read their own type
//! representation through a comptime context, and the walkers here decide
//! what the value means. This module depends on nothing else in the
//! compiler, so the standalone Boxy runtime object can import it too.

pub const shape = @import("shape.zig");
pub const inspect = @import("inspect.zig");
pub const inspect_override = @import("inspect_override.zig");
pub const structural = @import("structural.zig");

pub const ShapeKind = shape.ShapeKind;
pub const InspectShape = inspect.InspectShape;
pub const InspectSpec = inspect.InspectSpec;
pub const lookupInspectOverride = inspect_override.lookupInspectOverride;

test "value rules declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
    _ = @import("test.zig");
}
