//! `.boxy` runtime lowering: the layout-polymorphic strategy described in
//! design.md's "`.boxy` Runtime Lowering" section.

pub const Lower = @import("lower.zig");
pub const Types = @import("types.zig");
pub const Layouts = @import("layouts.zig");
pub const StaticDescs = @import("static_descs.zig");
/// Type tables: each generic worker's needs list and its recipes.
pub const Tables = @import("tables.zig");
pub const Body = @import("body.zig");
pub const Match = @import("match.zig");
/// Stored function values of compile-time evaluation.
pub const StoredFn = @import("stored_fn.zig");
/// The constant schema of the roots the checked-value writer completes.
pub const ValueSchema = @import("value_schema.zig").ValueSchema;
/// The host boundary of a platform entrypoint run from unit images.
pub const EntrypointSchema = @import("entrypoint_schema.zig").EntrypointSchema;

/// Lower checked modules to LIR under `.boxy`.
pub const run = Lower.run;
/// Everything one `.boxy` lowering reads.
pub const Input = Lower.Input;
/// The lowered program and the static data it carries.
pub const Output = Lower.Output;

test "boxy declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
