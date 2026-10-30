//! `.boxy` runtime lowering: the layout-polymorphic strategy described in
//! design.md's "`.boxy` Runtime Lowering" section.

pub const Lower = @import("lower.zig");
pub const Types = @import("types.zig");
pub const Layouts = @import("layouts.zig");
pub const StaticDescs = @import("static_descs.zig");
pub const Body = @import("body.zig");
pub const Match = @import("match.zig");

/// Lower checked modules to LIR under `.boxy`.
pub const run = Lower.run;
/// Everything one `.boxy` lowering reads.
pub const Input = Lower.Input;
/// The lowered program and the static data it carries.
pub const Output = Lower.Output;

test "boxy declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
